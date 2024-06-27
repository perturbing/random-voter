module Commands.GenerateVRFOutput (
    GenerateVRFOutputCommand,
    generateVRFOutputCommandParser,
    runGenerateVRFOutputCommand,
) where

import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as B
import Options.Applicative

data GenerateVRFOutputCommand = GenerateVRFOutputCommand
    { privateKey :: String
    , input :: String
    }

generateVRFOutputCommandParser :: Parser GenerateVRFOutputCommand
generateVRFOutputCommandParser =
    GenerateVRFOutputCommand
        <$> strOption
            ( long "private-key"
                <> metavar "PRIVATE_KEY"
                <> help "The private key for generating VRF output"
            )
        <*> strOption
            ( long "input"
                <> metavar "INPUT"
                <> help "The input data for VRF generation"
            )

runGenerateVRFOutputCommand :: GenerateVRFOutputCommand -> IO ()
runGenerateVRFOutputCommand cmd = do
    print $ privateKey cmd <> " " <> input cmd
