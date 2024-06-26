{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Api (
    IsPlutusScriptLanguage (..),
    PlutusScriptV1,
    PlutusScriptV2,
    PlutusScriptV3,
    PlutusScriptVersion (..),
    Script (..),
    ScriptHash (..),
    hashScript,
    prettyPrintJSON,
    writeFileTextEnvelope,
 )
import Cardano.Api.Shelley (
    File (..),
    PlutusScript (..),
    Script (..),
    fromPlutusData,
    scriptDataToJsonDetailedSchema,
    serialiseToRawBytes,
    unsafeHashableScriptData,
 )
import qualified PlutusLedgerApi.V1 as PlutusV1
import qualified PlutusLedgerApi.V2 as PlutusV2
import qualified PlutusLedgerApi.V3 as PlutusV3
import PlutusTx (CompiledCode, liftCodeDef, unsafeApplyCode)
import qualified PlutusTx.Builtins as PlutusTx

import Scripts (
    alwaysTrueMintCode,
    randomVoteCode,
 )

import Data.Aeson (Value, decode)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import GHC.ByteOrder (ByteOrder (..))
import qualified PlutusTx.Prelude as P

writePlutusScriptToFile :: (IsPlutusScriptLanguage lang) => FilePath -> PlutusScript lang -> IO ()
writePlutusScriptToFile filePath script =
    writeFileTextEnvelope (File filePath) Nothing script >>= \case
        Left err -> print "error writing script"
        Right () -> putStrLn $ "Serialized script to: " ++ filePath

writeCodeToFile :: forall lang a. PlutusScriptVersion lang -> FilePath -> CompiledCode a -> IO ()
writeCodeToFile version filePath = case version of
    PlutusScriptV1 -> writePlutusScriptToFile @PlutusScriptV1 filePath . PlutusScriptSerialised . PlutusV1.serialiseCompiledCode
    PlutusScriptV2 -> writePlutusScriptToFile @PlutusScriptV2 filePath . PlutusScriptSerialised . PlutusV2.serialiseCompiledCode
    PlutusScriptV3 -> writePlutusScriptToFile @PlutusScriptV3 filePath . PlutusScriptSerialised . PlutusV3.serialiseCompiledCode

---------------------------------------

scriptHashAlwaysTrueMint :: ScriptHash
scriptHashAlwaysTrueMint = hashScript . PlutusScript PlutusScriptV3 . PlutusScriptSerialised . PlutusV3.serialiseCompiledCode $ alwaysTrueMintCode

alwaysTrueCurrencySymbol :: PlutusV3.CurrencySymbol
alwaysTrueCurrencySymbol = PlutusV3.CurrencySymbol . PlutusV3.toBuiltin . serialiseToRawBytes $ scriptHashAlwaysTrueMint

dataToJSON :: (PlutusV3.ToData a) => a -> Value
dataToJSON = scriptDataToJsonDetailedSchema . unsafeHashableScriptData . fromPlutusData . PlutusV3.toData

printDataToJSON :: (PlutusV3.ToData a) => a -> IO ()
printDataToJSON = putStrLn . BS8.unpack . prettyPrintJSON . dataToJSON

initTxOutRef :: PlutusV3.TxOutRef
initTxOutRef =
    PlutusV3.TxOutRef
        ((PlutusV3.TxId . P.integerToByteString BigEndian 32) 0x7f6a7f48d447b1ae63ec2b8b1a623cca4d36838f8010e545c565c2d26ccb70f5)
        1

main :: IO ()
main = do
    writeCodeToFile PlutusScriptV3 "./assets/V3/alwaysTrueMint.plutus" alwaysTrueMintCode
    writeCodeToFile PlutusScriptV3 "./assets/V3/randomVoteCC.plutus" randomVoteCode
