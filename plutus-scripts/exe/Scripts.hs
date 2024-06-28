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
import Plutus.Crypto.VRF (Input (..), Output (..), Proof (..), PubKey (..), checkVRF)
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

{-# INLINEABLE findTxInByTxOutRef #-}
findTxInByTxOutRef :: TxOutRef -> TxInfo -> Maybe TxInInfo
findTxInByTxOutRef outRef TxInfo{txInfoInputs} =
    find
        (\TxInInfo{txInInfoOutRef} -> txInInfoOutRef == outRef)
        txInfoInputs

-- This is a simple voting script that allows for a random vote to be cast
-- by a DREP. It is parameterized by a VRF public key of the voter and
-- a TxOutRef that the DREP must spend to register the voter.
-- For intuition, see the VRF as a keyed hash function.
-- For a given input, a public key can hash the input to create a
-- random hash output. This hash is unique to the public key, and the
-- correctness of the hash can be verified by anyone with the public key
-- and the input with a proof (gamma, c, s).
{-# INLINEABLE randomVoterDrep #-}
randomVoterDrep :: (TxOutRef, PubKey) -> ScriptContext -> Bool
randomVoterDrep (ref, vrfPubKey) ctx = case scriptContextScriptInfo ctx of
    -- First we check that the script is used to vote
    -- Given its 'voter', we lookup on what this voter is voting
    VotingScript voter -> case lookup voter (txInfoVotes txInfo) of
        -- This is the Map GovernanceActionId Vote for our voter
        -- We check that the redeemer is a tuple of (output, gamma, c, s)
        -- Which is a proof for a single random vote
        Just votes -> case redeemer of
            Just (output@(Output out), proof) ->
                let
                    -- We only allow for one vote per transaction (for simplicity)
                    ((govAction, vote) : xs) = toList votes
                    -- create a byte string from the txId and the index of the vote
                    input = Input $ (getTxId . gaidTxId) govAction <> integerToByteString BigEndian 0 (gaidGovActionIx govAction)
                    -- map our random hash output to a vote (Yes, No, Abstain) with chance 1/3 for each
                    randomInt = byteStringToInteger BigEndian out `modulo` 3
                    randomVote
                        | randomInt == 0 = VoteYes
                        | randomInt == 1 = VoteNo
                        | otherwise = Abstain
                 in
                    -- Here we check that one vote is cast in the transaction
                    -- The cast vote is random and the proof is correct
                    -- given our input and hard-coded VRF public key
                    null xs && checkVRF vrfPubKey input output proof && vote == randomVote
            Nothing -> False
        Nothing -> False
    -- To register the drep, it must spend a specified txoutref
    CertifyingScript _ _ -> case findTxInByTxOutRef ref txInfo of
        Just _ -> True
        Nothing -> False
    _ -> False
  where
    txInfo = scriptContextTxInfo ctx
    redeemer :: Maybe (Output, Proof)
    redeemer = fromBuiltinData . getRedeemer $ scriptContextRedeemer ctx

{-# INLINEABLE wrappedRandomVoterDrep #-}
wrappedRandomVoterDrep :: BuiltinData -> BuiltinData -> BuiltinUnit
wrappedRandomVoterDrep = wrapTwoArgs randomVoterDrep

randomVoterDrepCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinUnit)
randomVoterDrepCode = $$(compile [||wrappedRandomVoterDrep||])
