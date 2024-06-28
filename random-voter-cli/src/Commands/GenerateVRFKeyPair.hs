module Commands.GenerateVRFKeyPair (
    GenerateVRFKeyPairCommand,
    generateVRFKeyPairCommandParser,
    runGenerateVRFKeyPairCommand,
) where

import Crypto.Random (getRandomBytes)
import Data.Aeson (object, (.=))
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Aeson.Key as Key
import Data.Binary (decode)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString.Lazy as B
import Data.Word (Word32)
import GHC.ByteOrder (ByteOrder (..))
import Options.Applicative
import PlutusTx.Builtins (
    BuiltinByteString,
    bls12_381_G1_compressed_generator,
    bls12_381_G1_scalarMul,
    bls12_381_G1_uncompress,
    byteStringToInteger,
 )
import PlutusTx.Prelude (modulo)

newtype GenerateVRFKeyPairCommand = GenerateVRFKeyPairCommand {keyName :: String}

generateVRFKeyPairCommandParser :: Parser GenerateVRFKeyPairCommand
generateVRFKeyPairCommandParser =
    GenerateVRFKeyPairCommand
        <$> strOption
            ( long "out-file"
                <> metavar "FILE_NAME"
                <> help "The name of the file to store the private and public key pair in JSON format"
            )

runGenerateVRFKeyPairCommand :: GenerateVRFKeyPairCommand -> IO ()
runGenerateVRFKeyPairCommand cmd = do
    let fileName = keyName cmd
    -- Generate a random 32-bit integer using cryptonite
    randomBytes <- getRandomBytes 32 :: IO BuiltinByteString
    let priv = byteStringToInteger BigEndian randomBytes `modulo` 52435875175126190479447740508185965837690552500527637822603658699938581184513
        g1 = bls12_381_G1_uncompress bls12_381_G1_compressed_generator
        pub = bls12_381_G1_scalarMul priv g1
        privString = show priv
        pubString = show pub
    -- Create JSON object
    let json =
            object
                [ Key.fromString "private_key" .= privString
                , Key.fromString "public_key" .= pubString
                ]
    B.writeFile fileName (encodePretty json)
    putStrLn $ "Generated private key and public key written to " ++ fileName
