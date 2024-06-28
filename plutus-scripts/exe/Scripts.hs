{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}

module Scripts where

import GHC.ByteOrder (ByteOrder (..))
import PlutusLedgerApi.V3 (
    GovernanceActionId (..),
    Redeemer (..),
    ScriptContext (..),
    ScriptInfo (..),
    ScriptPurpose (..),
    TxId (..),
    TxInInfo (..),
    TxInfo (..),
    TxOutRef (..),
    Vote (..),
    fromBuiltinData,
 )
import PlutusTx (
    CompiledCode,
    compile,
    makeIsDataIndexed,
 )
import PlutusTx.AssocMap (Map (..), all, lookup, mapWithKey, toList)
import PlutusTx.Bool (
    Bool (..),
    otherwise,
    (&&),
 )
import PlutusTx.Builtins (
    BuiltinByteString,
    BuiltinData,
    Integer,
    bls12_381_G1_add,
    bls12_381_G1_compress,
    bls12_381_G1_compressed_generator,
    bls12_381_G1_hashToGroup,
    bls12_381_G1_scalarMul,
    bls12_381_G1_uncompress,
    byteStringToInteger,
    error,
    integerToByteString,
    sha2_256,
 )
import PlutusTx.Prelude (
    BuiltinUnit,
    Maybe (..),
    find,
    mconcat,
    mempty,
    modulo,
    null,
    ($),
    (.),
    (<$>),
    (<>),
    (==),
 )
import Shared (wrapFourArgs, wrapOneArg, wrapThreeArgs, wrapTwoArgs)

-- This is a simple voting script that allows for a random vote to be cast
-- by two of the tree kinds of voters: "CC", "DREP" (not a stake pool).
-- It is parameterized by a VRF public key of the voter.
-- For intuition, see the VRF as a keyed hash function.
-- For a given input, a public key can hash the input to create a
-- random hash output. This hash is unique to the public key, and the
-- correctness of the hash can be verified by anyone with the public key
-- and the input with a proof (gamma, c, s).
{-# INLINEABLE randomVoter #-}
randomVoter :: BuiltinByteString -> ScriptContext -> Bool
randomVoter vrfPubKey ctx = case scriptContextScriptInfo ctx of
    -- First we check that the script is used to vote
    -- Given its 'voter', we lookup on what this voter is voting
    VotingScript voter -> case lookup voter (txInfoVotes txInfo) of
        -- This is the Map GovernanceActionId Vote for our voter
        -- We check that the redeemer is a tuple of (output, gamma, c, s)
        -- Which is a proof for a single random vote
        Just votes -> case redeemer of
            Just (output, gammaBS, c, s) ->
                let
                    -- We only allow for one vote per transaction (for simplicity)
                    ((govAction, vote) : xs) = toList votes
                    -- create a byte string from the txId and the index of the vote
                    input = (getTxId . gaidTxId) govAction <> integerToByteString BigEndian 0 (gaidGovActionIx govAction)
                    -- map our random hash output to a vote (Yes, No, Abstain) with chance 1/3 for each
                    randomInt = byteStringToInteger BigEndian output `modulo` 3
                    randomVote
                        | randomInt == 0 = VoteYes
                        | randomInt == 1 = VoteNo
                        | otherwise = Abstain
                 in
                    -- Here we check that one vote is cast in the transaction
                    -- The cast vote is random and the proof is correct
                    -- given our input and hard-coded VRF public key
                    null xs && checkVRF input output (gammaBS, c, s) && vote == randomVote
            Nothing -> False
        Nothing -> False
    _ -> False
  where
    txInfo = scriptContextTxInfo ctx
    redeemer :: Maybe (BuiltinByteString, BuiltinByteString, Integer, Integer)
    redeemer = fromBuiltinData . getRedeemer $ scriptContextRedeemer ctx
    -- see https://gist.github.com/perturbing/ebde137286944b30b1de2277cfaf1c5a
    checkVRF :: BuiltinByteString -> BuiltinByteString -> (BuiltinByteString, Integer, Integer) -> Bool
    checkVRF input output (gammaBS, c, s) =
        let pub = bls12_381_G1_uncompress vrfPubKey
            g1 = bls12_381_G1_uncompress bls12_381_G1_compressed_generator
            gamma = bls12_381_G1_uncompress gammaBS
            u = bls12_381_G1_add (bls12_381_G1_scalarMul c pub) (bls12_381_G1_scalarMul s g1)
            h' = bls12_381_G1_hashToGroup input mempty
            v = bls12_381_G1_add (bls12_381_G1_scalarMul c gamma) (bls12_381_G1_scalarMul s h')
         in integerToByteString BigEndian 32 c
                == (sha2_256 . mconcat $ [bls12_381_G1_compressed_generator, bls12_381_G1_compress h', vrfPubKey, gammaBS, bls12_381_G1_compress u, bls12_381_G1_compress v])
                && output
                == sha2_256 gammaBS

{-# INLINEABLE wrappedRandomVoter #-}
wrappedRandomVoter :: BuiltinData -> BuiltinData -> BuiltinUnit
wrappedRandomVoter = wrapTwoArgs randomVoter

randomVoterCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinUnit)
randomVoterCode = $$(compile [||wrappedRandomVoter||])

-- Testing purposes

{-# INLINEABLE alwaysTrueMint #-}
alwaysTrueMint :: BuiltinData -> Bool
alwaysTrueMint _ = True

{-# INLINEABLE wrappedAlwaysTrueMint #-}
wrappedAlwaysTrueMint :: BuiltinData -> BuiltinUnit
wrappedAlwaysTrueMint = wrapOneArg alwaysTrueMint

alwaysTrueMintCode :: CompiledCode (BuiltinData -> BuiltinUnit)
alwaysTrueMintCode = $$(compile [||wrappedAlwaysTrueMint||])
