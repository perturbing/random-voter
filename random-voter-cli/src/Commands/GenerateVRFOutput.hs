module Commands.GenerateVRFOutput (
    GenerateVRFOutputCommand,
    generateVRFOutputCommandParser,
    runGenerateVRFOutputCommand,
) where

import Cardano.Api (
    prettyPrintJSON,
 )
import Cardano.Api.Shelley (
    fromPlutusData,
    scriptDataToJsonDetailedSchema,
    unsafeHashableScriptData,
 )
import Control.Exception (SomeException, catch, evaluate, try)
import Crypto.Random (getRandomBytes)
import Data.Aeson (Value, decode, eitherDecode, withObject, (.:))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char (digitToInt)
import qualified Data.Maybe as Maybe
import Data.Text (Text, pack, unpack)
import GHC.ByteOrder (ByteOrder (..))
import Numeric (readHex)
import Options.Applicative
import Plutus.Crypto.VRF (Input (..), Output (..), PrivKey (..), Proof (..), PubKey (..), runVRF)
import qualified PlutusLedgerApi.V3 as PlutusV3
import PlutusTx.Builtins (
    BuiltinBLS12_381_G1_Element,
    BuiltinByteString,
    bls12_381_G1_compressed_generator,
    bls12_381_G1_scalarMul,
    bls12_381_G1_uncompress,
    byteStringToInteger,
    fromBuiltin,
    integerToByteString,
    toBuiltin,
 )
import PlutusTx.Prelude (modulo)

data GenerateVRFOutputCommand = GenerateVRFOutputCommand
    { keyFileName :: String
    , txId :: String
    , index :: Integer
    , outFileName :: String
    }

generateVRFOutputCommandParser :: Options.Applicative.Parser GenerateVRFOutputCommand
generateVRFOutputCommandParser =
    GenerateVRFOutputCommand
        <$> strOption
            ( long "key-pair-file"
                <> metavar "KEY_PAIR_FILE"
                <> help "The file containing the private and public key pair in JSON format"
            )
        <*> strOption
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
        <*> strOption
            ( long "out-file"
                <> metavar "FILE_NAME"
                <> help "The name of the file to store the redeemer in JSON format"
            )

hexToBytes :: String -> BuiltinByteString
hexToBytes = toBuiltin . BS.pack . hexStringToBytes . dropPrefix
  where
    dropPrefix ('0' : 'x' : xs) = xs
    dropPrefix xs = xs
    hexStringToBytes [] = []
    hexStringToBytes (x : y : xs) = fromIntegral ((digitToInt x * 16) + digitToInt y) : hexStringToBytes xs
    hexStringToBytes _ = error "Invalid hex string"

runGenerateVRFOutputCommand :: GenerateVRFOutputCommand -> IO ()
runGenerateVRFOutputCommand cmd = do
    let keyFile = keyFileName cmd
        txIdAction = hexToBytes $ txId cmd
        indexAction = index cmd
        outFile = outFileName cmd
    -- Read and decode the key pair file
    keyFileContent <- B.readFile keyFile
    let eitherKeys = eitherDecode keyFileContent :: Either String Aeson.Object
    case eitherKeys of
        Left err -> print $ "Error parsing JSON: " ++ err
        Right keys -> do
            -- Extract private_key and public_key from JSON
            let privateKeyStr = Maybe.fromMaybe (error "No private_key found") (Aeson.parseMaybe (.: Key.fromString "private_key") keys) :: String
                publicKeyStr = Maybe.fromMaybe (error "No public_key found") (Aeson.parseMaybe (.: Key.fromString "public_key") keys) :: String
            -- Convert private_key to Integer
            let privateKey = read privateKeyStr :: Integer
                privKey = PrivKey $ integerToByteString BigEndian 32 privateKey
                publicKey = hexToBytes publicKeyStr
            -- Handle possible exceptions from bls12_381_G1_uncompress
            pubResult <- try (evaluate (bls12_381_G1_uncompress publicKey)) :: IO (Either SomeException BuiltinBLS12_381_G1_Element)
            case pubResult of
                Left ex -> print "Error during public key uncompression, the public key is invalid."
                Right pub -> do
                    let publicKeyCalculated = bls12_381_G1_scalarMul privateKey (bls12_381_G1_uncompress bls12_381_G1_compressed_generator)
                    if publicKeyCalculated == pub
                        then do
                            let input = txIdAction <> integerToByteString BigEndian 0 indexAction
                            kBs <- getRandomBytes 32 :: IO BuiltinByteString
                            let k = byteStringToInteger BigEndian kBs `modulo` 52435875175126190479447740508185965837690552500527637822603658699938581184513
                                (Output output, proof) = runVRF privKey k (Input input)
                                randomInt = byteStringToInteger BigEndian output `modulo` 3
                                randomVote
                                    | randomInt == 0 = PlutusV3.VoteYes
                                    | randomInt == 1 = PlutusV3.VoteNo
                                    | otherwise = PlutusV3.Abstain
                            print $ "You should vote '" <> show randomVote <> "' for the governance action: " <> txId cmd <> "#" <> show indexAction
                            -- writeDataToJSON outFile (Output output, proof)
                            writeFile outFile $ BS8.unpack . prettyPrintJSON $ dataToJSON (Output output, proof)
                            print $ "Redeemer written to: " <> outFile
                        else print "Claimed public key does not match the one derived from the private key."

dataToJSON :: (PlutusV3.ToData a) => a -> Value
dataToJSON = scriptDataToJsonDetailedSchema . unsafeHashableScriptData . fromPlutusData . PlutusV3.toData
