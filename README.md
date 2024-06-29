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
"You should vote 'VoteNo' for the governance action: fe2c99fe6bc75a9666427163d51ae7dbf5a60df40135361b7bfd53ac6c7912ec#3"
"Redeemer written to: redeemer.json"
```
This will also write a redeemer file to disk, which can be used to witness your Drep/CC member when sending the transaction that votes on the above governance action. Note that both scripts only allow you to vote on one governance action at the time. Please have no hesitation to play around with the index or tx-id, and see how your vote would change given a different input. Similar to a normal hash function, this output looks random and unpredictable.