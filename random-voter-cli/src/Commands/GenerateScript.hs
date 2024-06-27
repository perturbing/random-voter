module Commands.GenerateScript (
    GenerateScriptCommand,
    generateScriptCommandParser,
    runGenerateScriptCommand,
) where

import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as B
import Options.Applicative

data GenerateScriptCommand = GenerateScriptCommand
    { privateKey :: String
    , input :: String
    }

generateScriptCommandParser :: Parser GenerateScriptCommand
generateScriptCommandParser =
    GenerateScriptCommand
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

runGenerateScriptCommand :: GenerateScriptCommand -> IO ()
runGenerateScriptCommand cmd = do
    -- todo
    print "Generating VRF output..."
