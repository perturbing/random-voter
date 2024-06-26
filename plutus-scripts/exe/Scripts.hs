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

-- see https://gist.github.com/perturbing/ebde137286944b30b1de2277cfaf1c5a
{-# INLINEABLE randomVoteCC #-}
randomVoteCC :: BuiltinByteString -> ScriptContext -> Bool
randomVoteCC vrfPubKey ctx = case scriptContextScriptInfo ctx of
    VotingScript voter -> case lookup voter (txInfoVotes txInfo) of
        -- This is a Map GovernanceActionId Vote
        Just votes -> case redeemer of
            Just (output, gammaBS, c, s) ->
                let ((govAction, vote) : xs) = toList votes
                    input = (getTxId . gaidTxId) govAction <> integerToByteString BigEndian 0 (gaidGovActionIx govAction)
                    randomInt = byteStringToInteger BigEndian output `modulo` 3
                    randomVote
                        | randomInt == 0 = VoteYes
                        | randomInt == 1 = VoteNo
                        | otherwise = Abstain
                 in null xs && checkVRF input output (gammaBS, c, s) && vote == randomVote
            Nothing -> False
        Nothing -> False
    _ -> False
  where
    txInfo = scriptContextTxInfo ctx
    redeemer :: Maybe (BuiltinByteString, BuiltinByteString, Integer, Integer)
    redeemer = fromBuiltinData . getRedeemer $ scriptContextRedeemer ctx
    checkVRF :: BuiltinByteString -> BuiltinByteString -> (BuiltinByteString, Integer, Integer) -> Bool
    checkVRF input output (gammaBS, c, s) =
        let pub = bls12_381_G1_uncompress vrfPubKey
            g1 = bls12_381_G1_uncompress bls12_381_G1_compressed_generator
            gamma = bls12_381_G1_uncompress gammaBS
            u = bls12_381_G1_add (bls12_381_G1_scalarMul c pub) (bls12_381_G1_scalarMul s g1)
            h' = bls12_381_G1_hashToGroup input mempty
            v = bls12_381_G1_add (bls12_381_G1_scalarMul c gamma) (bls12_381_G1_scalarMul s h')
            -- this is the cofactor of G1
            f = 76329603384216526031706109802092473003
         in integerToByteString BigEndian 32 c
                == (sha2_256 . mconcat $ bls12_381_G1_compress <$> [g1, h', pub, gamma, u, v])
                && output
                == (sha2_256 . bls12_381_G1_compress . bls12_381_G1_scalarMul f) gamma

{-# INLINEABLE wrappedRandomVoteCC #-}
wrappedRandomVoteCC :: BuiltinData -> BuiltinData -> BuiltinUnit
wrappedRandomVoteCC = wrapTwoArgs randomVoteCC

randomVoteCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinUnit)
randomVoteCode = $$(compile [||wrappedRandomVoteCC||])

-- Testing purposes

{-# INLINEABLE alwaysTrueMint #-}
alwaysTrueMint :: BuiltinData -> Bool
alwaysTrueMint _ = True

{-# INLINEABLE wrappedAlwaysTrueMint #-}
wrappedAlwaysTrueMint :: BuiltinData -> BuiltinUnit
wrappedAlwaysTrueMint = wrapOneArg alwaysTrueMint

alwaysTrueMintCode :: CompiledCode (BuiltinData -> BuiltinUnit)
alwaysTrueMintCode = $$(compile [||wrappedAlwaysTrueMint||])
