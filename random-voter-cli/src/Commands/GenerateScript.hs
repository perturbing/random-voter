{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Commands.GenerateScript (
    GenerateScriptCommand,
    generateScriptCommandParser,
    runGenerateScriptCommand,
) where

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
import Control.Applicative (asum, (<**>))
import Control.Exception (SomeException, catch, evaluate, try)
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char (digitToInt)
import Options.Applicative
import qualified PlutusLedgerApi.V1 as PlutusV1
import qualified PlutusLedgerApi.V2 as PlutusV2
import qualified PlutusLedgerApi.V3 as PlutusV3
import PlutusTx (CompiledCode, liftCodeDef, unsafeApplyCode)
import PlutusTx.Builtins (
    BuiltinBLS12_381_G1_Element,
    BuiltinByteString,
    bls12_381_G1_compress,
    bls12_381_G1_compressed_generator,
    bls12_381_G1_scalarMul,
    bls12_381_G1_uncompress,
    byteStringToInteger,
    fromBuiltin,
    integerToByteString,
    toBuiltin,
 )
import qualified PlutusTx.Builtins as PlutusTx
import Text.Read (readEither)

import qualified Plutus.Crypto.VRF as VRF
import Scripts (randomVoterCCCode, randomVoterDrepCode)

data Voter = Drep {txId :: String, index :: Integer} | CC
    deriving (Show, Read)

data GenerateScriptCommand = GenerateScriptCommand
    { keyFileName :: String
    , voter :: Voter
    , output :: String
    }

generateScriptCommandParser :: Parser GenerateScriptCommand
generateScriptCommandParser =
    GenerateScriptCommand
        <$> strOption
            ( long "public-key"
                <> metavar "HEX_VRF_KEY_STRING"
                <> help "The public key of the VRF key pair in hex format"
            )
        <*> voterParser
        <*> strOption
            ( long "out-file"
                <> metavar "FILE_NAME"
                <> help "The name of the file to store the redeemer in JSON format"
            )

voterParser :: Parser Voter
voterParser =
    subparser
        ( command
            "drep"
            ( info
                ( Drep
                    <$> strOption
                        ( long "tx-id"
                            <> metavar "REF_TX_ID"
                            <> help "The Tx Ref of the governance action"
                        )
                    <*> option
                        auto
                        ( long "index"
                            <> metavar "INDEX"
                            <> help "The index of the governance action"
                        )
                )
                (progDesc "Voter type: Drep")
            )
            <> command
                "cc"
                ( info
                    (pure CC)
                    (progDesc "Voter type: CC")
                )
        )

hexToBytes :: String -> BuiltinByteString
hexToBytes = toBuiltin . BS.pack . hexStringToBytes . dropPrefix
  where
    dropPrefix ('0' : 'x' : xs) = xs
    dropPrefix xs = xs
    hexStringToBytes [] = []
    hexStringToBytes (x : y : xs) = fromIntegral ((digitToInt x * 16) + digitToInt y) : hexStringToBytes xs
    hexStringToBytes _ = error "Invalid hex string"

runGenerateScriptCommand :: GenerateScriptCommand -> IO ()
runGenerateScriptCommand cmd = do
    let publicKey = hexToBytes $ keyFileName cmd
    pubResult <- try (evaluate (bls12_381_G1_uncompress publicKey)) :: IO (Either SomeException BuiltinBLS12_381_G1_Element)
    case pubResult of
        Left e -> print "Invalid public key provided"
        Right pubKey -> case voter cmd of
            Drep txId' index -> do
                let txId = hexToBytes txId'
                    initTxOutRef = PlutusV3.TxOutRef (PlutusV3.TxId txId) index
                    vrfPubKey = VRF.PubKey $ bls12_381_G1_compress pubKey
                    appliedScript = randomVoterDrepCode `unsafeApplyCode` PlutusTx.liftCodeDef (PlutusV3.toBuiltinData (initTxOutRef, vrfPubKey))
                writeCodeToFile PlutusScriptV3 (output cmd) appliedScript
            CC -> do
                let vrfPubKey = VRF.PubKey $ bls12_381_G1_compress pubKey
                    appliedScript = randomVoterCCCode `unsafeApplyCode` PlutusTx.liftCodeDef (PlutusV3.toBuiltinData vrfPubKey)
                writeCodeToFile PlutusScriptV3 (output cmd) appliedScript

opts :: ParserInfo GenerateScriptCommand
opts =
    info
        (generateScriptCommandParser <**> helper)
        ( fullDesc
            <> progDesc "Generate a script with the provided public key and utxo"
            <> header "generate-script - a script generation tool"
        )

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
