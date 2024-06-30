# random-voter
A gimmick plutus based CC or DREP that votes random on a governance actions.

## The what
This repository contains two plutus scripts in the file `/plutus-scripts/Scripts.hs` that implement a "randomly" voting Drep and CC credential. At its core, this functionality is achieved via VRF over the G1 subgroup of the BLS12-381 curve. The intuition behind this is that the scripts that represent either the CC member or the DRep, hard-code a certain VRF public key. With the corresponding private key, they can hash input values, which can be verified against the public key. An important property of such a VRF/"keyed hash function", is that, given an input, there exist only one hash digest which they can prove. This property is leveraged to effectively let the DRep/CC members vote randomly, as the plutus scripts checks that their vote is determined by their deterministic keyed hash of the GovAction ID and index (which is unique).

## The how
To create a VRF key over the BLS curve, you can do the following command
```bash
nix run .#random-voter-cli -- generate-keypair --out-file my-vrf-key.json
```
which will create something like this
```bash
{
    "private_key": "23608682348644603384365127525591197063375029956275631844059539617124428654050",
    "public_key": "0x9867bc43ae375b837f7ed1b49abf26c53ffc8c6592c57f80140b9d17df133805ba740bdb0766f4d3b5dd250190db5c78"
}
```
Keep this private key to yourself. Then, you can compile with this key file a Drep or CC member script that binds to this VRF key via
```bash
nix run .#random-voter-cli -- generate-script --public-key $(cat my-vrf-key.json | jq -r '.public_key') cc --out-file random-voting-cc-script.plutus
```
and for DReps
```bash
nix run .#random-voter-cli -- generate-script --public-key $(cat my-vrf-key.json | jq -r '.public_key') drep --tx-id 8ee39511cecbffbb9c216447d88093cd4a0762d6f3da1f952dad1007dc267134 --index 0 --out-file random-voting-drep-script.plutus
```
Note that the Drep command also requires an UTxO reference, this UTxO needs to be spent to register the Drep. As this is a POC gimmick example, the functionality for deregistering is not implemented. 

Given a governance action `fe2c99fe6bc75a9666427163d51ae7dbf5a60df40135361b7bfd53ac6c7912ec#3` you can calculate how you should vote (there is only one way, which is random but deterministic of the reference used), you can use
```bash
nix run .#random-voter-cli -- generate-vrf-output-redeemer --key-pair-file my-vrf-key.json --tx-id fe2c99fe6bc75a9666427163d51ae7dbf5a60df40135361b7bfd53ac6c7912ec --index 3 --out-file redeemer.json
```
which will output something like
```bash
"You should vote 'VoteNo' for the governance action: fe2c99fe6bc75a9666427163d51ae7dbf5a60df40135361b7bfd53ac6c7912ec#3"
"Redeemer written to: redeemer.json"
```
This will also write a redeemer file to disk, which can be used to witness your Drep/CC member when sending the transaction that votes on the above governance action. Note that both scripts only allow you to vote on one governance action at the time. Please have no hesitation to play around with the index or tx-id, and see how your vote would change given a different input. Similar to a normal hash function, this output looks random and unpredictable.

## E2E example
This repository also provides a nix shell via `nix develop`. In it, a script `deploy-local-testnet` is present which deploys a local testnet with plutus V3 enabled and in the Conway era (PV 10). If you want to reset this testnet, use `purge-local-testnet`. The hard-coded cc member is the always true script, and in the following example we will set up an environment where we utilize both the random voting cc script (as the hot credential) and a random voting drep. So to start, enter a shell and start the local testnet via `deploy-local-testnet`, open a new terminal and also enter a shell there. In this example, we use these keys, which already have some ada. We also make a directory in which we will perform all commands.
```bash
mkdir example
cd example
cp ../local-testnet/example/utxo-keys/utxo1.skey ./payment.skey
cp ../local-testnet/example/utxo-keys/utxo1.vkey ./payment.vkey
cardano-cli address build --testnet-magic 42 --payment-verification-key-file payment.vkey > payment.addr
cardano-cli query utxo --testnet-magic 42 --address $(cat payment.addr)
```
Next we generate a cc vrf key via
```bash
nix run .#random-voter-cli -- generate-keypair --out-file cc-vrf-key.json
```
and a cc hot credential that votes randomly via
```bash
nix run .#random-voter-cli -- generate-script --public-key $(cat cc-vrf-key.json | jq -r '.public_key') cc --out-file cc-hot.plutus
```
Notice that given the output of
```bash
cardano-cli conway query committee-state --testnet-magic 42
```
We still need to authorize the hot credential from the cold "always true script" credential. So we make such a certificate
```bash
cardano-cli conway governance committee create-hot-key-authorization-certificate --cold-script-file ../data/always-true.plutus --hot-script-file cc-hot.plutus --out-file cc-hot-auth.cert
```
which we witness and send via
```bash
cardano-cli conway transaction build --testnet-magic 42 \
 --tx-in "$(cardano-cli query utxo --address "$(cat payment.addr)" --testnet-magic 42 --out-file /dev/stdout | jq -r 'keys[0]')" \
 --tx-in-collateral "$(cardano-cli query utxo --address "$(cat payment.addr)" --testnet-magic 42 --out-file /dev/stdout | jq -r 'keys[0]')" \
 --certificate-file cc-hot-auth.cert \
 --certificate-script-file ../data/always-true.plutus \
 --certificate-redeemer-value {} \
 --change-address $(cat payment.addr) \
 --out-file tx.raw
cardano-cli transaction sign --testnet-magic 42 \
 --signing-key-file payment.skey \
 --tx-body-file tx.raw \
 --out-file tx.signed
cardano-cli transaction submit --testnet-magic 42 --tx-file tx.signed
```

Next we create a drep vrf key via
```bash
nix run .#random-voter-cli -- generate-keypair --out-file drep-vrf-key.json
```
Then using the utxo at
```bash
cardano-cli query utxo --testnet-magic 42 --address $(cat payment.addr)
```
we create a script for the drep via
```bash
nix run .#random-voter-cli -- generate-script --public-key $(cat drep-vrf-key.json | jq -r '.public_key') drep --tx-id txIdHere --index txIndexHere --out-file random-voting-drep-script.plutus
```
Then we can register this drep script via the cert
```bash
cardano-cli conway governance drep registration-certificate \
--drep-script-hash $(cardano-cli transaction policyid --script-file random-voting-drep-script.plutus) \
--key-reg-deposit-amt $(cardano-cli conway query gov-state --testnet-magic 42 | jq -r .currentPParams.dRepDeposit) \
--out-file drep-register.cert
```
and witness it via 
```bash
cardano-cli conway transaction build --testnet-magic 42 \
 --tx-in $(cardano-cli query utxo --address $(cat payment.addr) --output-json --testnet-magic 42 | jq -r 'keys[0]') \
 --tx-in-collateral $(cardano-cli query utxo --address $(cat payment.addr) --output-json --testnet-magic 42 | jq -r 'keys[0]') \
 --certificate-file drep-register.cert \
 --certificate-script-file random-voting-drep-script.plutus \
 --certificate-redeemer-value {} \
 --change-address $(cat payment.addr) \
 --out-file tx
cardano-cli transaction sign --testnet-magic 42 --signing-key-file payment.skey --tx-body-file tx --out-file tx.signed
cardano-cli transaction submit --testnet-magic 42 --tx-file tx.signed
```
Now we still need to delegate stake to this drep, for this we first need to register a stake address
```bash
cardano-cli conway stake-address key-gen \
--verification-key-file stake.vkey \
--signing-key-file stake.skey
```
And then build a combined address
```bash
cardano-cli conway address build \
--payment-verification-key-file payment.vkey \
--stake-verification-key-file stake.vkey \
--out-file payment-stake.addr \
--testnet-magic 42
```
create a stake registration cert
```bash
cardano-cli conway stake-address registration-certificate \
--stake-verification-key-file stake.vkey \
--key-reg-deposit-amt $(cardano-cli conway query gov-state --testnet-magic 42 | jq .currentPParams.stakeAddressDeposit) \
--out-file registration.cert
```
and submit it with the funds returning to our new stake address
```bash
cardano-cli conway transaction build \
--testnet-magic 42 \
--witness-override 2 \
--tx-in $(cardano-cli query utxo --address $(cat payment.addr) --testnet-magic 42 --out-file  /dev/stdout | jq -r 'keys[0]') \
--change-address $(cat payment-stake.addr) \
--certificate-file registration.cert \
--out-file tx.raw
cardano-cli conway transaction sign \
--tx-body-file tx.raw \
--signing-key-file payment.skey \
--signing-key-file stake.skey \
--testnet-magic 42 \
--out-file tx.signed
cardano-cli conway transaction submit \
--testnet-magic 42 \
--tx-file tx.signed
```
we can then delegae our voting power from this stake key to the random voting drep via
```bash
cardano-cli conway stake-address vote-delegation-certificate \
--stake-verification-key-file stake.vkey \
--drep-script-hash $(cardano-cli transaction policyid --script-file random-voting-drep-script.plutus) \
--out-file vote-deleg.cert
```
and sign and submit it via
```bash
cardano-cli conway transaction build \
--testnet-magic 42 \
--witness-override 2 \
--tx-in $(cardano-cli query utxo --address $(cat payment-stake.addr) --testnet-magic 42 --out-file  /dev/stdout | jq -r 'keys[0]') \
--change-address $(cat payment-stake.addr) \
--certificate-file vote-deleg.cert \
--out-file tx.raw
cardano-cli conway transaction sign \
--tx-body-file tx.raw \
--signing-key-file payment.skey \
--signing-key-file stake.skey \
--testnet-magic 42 \
--out-file tx.signed
cardano-cli conway transaction submit \
--testnet-magic 42 \
--tx-file tx.signed
```
After some epochs, when the stake key and the drep are both registered and the stake is counted, it will appear via
```bash
cardano-cli conway query drep-stake-distribution --testnet-magic 42 --all-dreps
```
Then to vote with both the random voting CC member and random voting Drep, we create a bogus governance action to vote on via
```bash
cardano-cli conway governance action create-constitution \
  --testnet \
  --governance-action-deposit $(cardano-cli conway query gov-state --testnet-magic 42 | jq -r '.currentPParams.govActionDeposit') \
  --deposit-return-stake-verification-key-file stake.vkey \
  --anchor-url https://raw.githubusercontent.com/cardano-foundation/CIPs/master/CIP-0100/cip-0100.common.schema.json \
  --anchor-data-hash "9d99fbca260b2d77e6d3012204e1a8658f872637ae94cdb1d8a53f4369400aa9" \
  --constitution-url https://ipfs.io/ipfs/Qmdo2J5vkGKVu2ur43PuTrM7FdaeyfeFav8fhovT6C2tto \
  --constitution-hash "579da00778a5b4567c94630399203935f7d84bb2c457e56537e36a56ff490a4a" \
  --constitution-script-hash "edcd84c10e36ae810dc50847477083069db796219b39ccde790484e0" \
  --out-file action.gov
```
Send it
```bash
cardano-cli conway transaction build \
  --testnet-magic 42 \
  --tx-in "$(cardano-cli query utxo --address "$(cat payment-stake.addr)" --testnet-magic 42 --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address $(cat payment-stake.addr) \
  --proposal-file action.gov \
  --out-file tx.raw
cardano-cli conway transaction sign \
--tx-body-file tx.raw \
--signing-key-file payment.skey \
--signing-key-file stake.skey \
--testnet-magic 42 \
--out-file tx.signed
cardano-cli conway transaction submit \
--testnet-magic 42 \
--tx-file tx.signed
```
Next we vote for the cc on this gov action, but first we check how, given this gov action tx id and index, it should vote
```bash
nix run .#random-voter-cli -- generate-vrf-output-redeemer --key-pair-file cc-vrf-key.json --tx-id $(cardano-cli conway query gov-state --testnet-magic 42 | jq -r '.proposals[0].actionId.txId') --index $(cardano-cli conway query gov-state --testnet-magic 42 | jq -r '.proposals[0].actionId.govActionIx') --out-file redeemer.json
```
In this particular case, it returns
```bash
"You should vote 'Abstain' for the governance action: c716fdc0b7c811d742f55104e7b044c5091d2ba2aaf3a6e1d0dde8742c27e7fa#0"
"Redeemer written to: redeemer.json"
```
So we create a vote, that abstains
```bash
cardano-cli conway governance vote create \
    --abstain \
    --governance-action-tx-id "$(cardano-cli conway query gov-state --testnet-magic 42 | jq -r '.proposals[0].actionId.txId')" \
    --governance-action-index "$(cardano-cli conway query gov-state --testnet-magic 42 | jq -r '.proposals[0].actionId.govActionIx')" \
    --cc-hot-script-hash  $(cardano-cli transaction policyid --script-file cc-hot.plutus)\
    --out-file govAction.vote
```
And send it via
```bash
cardano-cli query protocol-parameters --testnet-magic 42 --out-file pparams.json
cardano-cli conway transaction build-raw \
 --tx-in "$(cardano-cli query utxo --address "$(cat payment-stake.addr)" --testnet-magic 42 --out-file /dev/stdout | jq -r 'keys[0]')" \
 --tx-in-collateral "$(cardano-cli query utxo --address "$(cat payment-stake.addr)" --testnet-magic 42 --out-file /dev/stdout | jq -r 'keys[0]')" \
 --vote-file govAction.vote \
 --vote-script-file cc-hot.plutus \
 --vote-redeemer-file redeemer.json \
 --vote-execution-units "(6000000000,4000000)" \
 --tx-out $(cat payment-stake.addr)+549493890015 \
 --fee 5000000 \
 --protocol-params-file pparams.json \
 --out-file tx.raw
cardano-cli conway transaction sign \
--tx-body-file tx.raw \
--signing-key-file payment.skey \
--testnet-magic 42 \
--out-file tx.signed
cardano-cli conway transaction submit \
--testnet-magic 42 \
--tx-file tx.signed
```
If we now do
```bash
cardano-cli conway query gov-state --testnet-magic 42 | jq -r '.proposals'
```
We see something like
```bash
"committeeVotes": {
      "scriptHash-22a087c1b6ea5e810d5a7d1f21da220f0c8be331afb8ed6587fa952b": "Abstain"
}
```
The random cc member voted!

Now for the drep we first check how we should vote via
```bash
nix run .#random-voter-cli -- generate-vrf-output-redeemer --key-pair-file drep-vrf-key.json --tx-id $(cardano-cli conway query gov-state --testnet-magic 42 | jq -r '.proposals[0].actionId.txId') --index $(cardano-cli conway query gov-state --testnet-magic 42 | jq -r '.proposals[0].actionId.govActionIx') --out-file redeemer.json
```
After which we can create a vote via
```bash
cardano-cli conway governance vote create \
    --yes \
    --governance-action-tx-id "$(cardano-cli conway query gov-state --testnet-magic 42 | jq -r '.proposals[0].actionId.txId')" \
    --governance-action-index "$(cardano-cli conway query gov-state --testnet-magic 42 | jq -r '.proposals[0].actionId.govActionIx')" \
    --drep-script-hash $(cardano-cli transaction policyid --script-file random-voting-drep-script.plutus)\
    --out-file govAction.vote
```
and send it via
```bash
cardano-cli query protocol-parameters --testnet-magic 42 --out-file pparams.json
cardano-cli conway transaction build-raw \
 --tx-in "$(cardano-cli query utxo --address "$(cat payment-stake.addr)" --testnet-magic 42 --out-file /dev/stdout | jq -r 'keys[0]')" \
 --tx-in-collateral "$(cardano-cli query utxo --address "$(cat payment-stake.addr)" --testnet-magic 42 --out-file /dev/stdout | jq -r 'keys[0]')" \
 --vote-file govAction.vote \
 --vote-script-file random-voting-drep-script.plutus \
 --vote-redeemer-file redeemer.json \
 --vote-execution-units "(6000000000,4000000)" \
 --tx-out $(cat payment-stake.addr)+549488890015 \
 --fee 5000000 \
 --protocol-params-file pparams.json \
 --out-file tx.raw
cardano-cli conway transaction sign \
--tx-body-file tx.raw \
--signing-key-file payment.skey \
--testnet-magic 42 \
--out-file tx.signed
cardano-cli conway transaction submit \
--testnet-magic 42 \
--tx-file tx.signed
```
And if we again take a look at
```bash
cardano-cli conway query gov-state --testnet-magic 42 | jq -r '.proposals'
```
We see that the drep also voted.