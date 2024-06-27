module Commands.GenerateVRFKeyPair (
    GenerateVRFKeyPairCommand,
    generateVRFKeyPairCommandParser,
    runGenerateVRFKeyPairCommand,
) where

import Options.Applicative

data GenerateVRFKeyPairCommand = GenerateVRFKeyPairCommand
    { privateKeyName :: String
    , publicKeyName :: String
    }

generateVRFKeyPairCommandParser :: Parser GenerateVRFKeyPairCommand
generateVRFKeyPairCommandParser =
    GenerateVRFKeyPairCommand
        <$> strOption
            ( long "private-key-name"
                <> metavar "PRIVATE_KEY_NAME"
                <> help "The name of the private key file"
            )
        <*> strOption
            ( long "public-key-name"
                <> metavar "PUBLIC_KEY_NAME"
                <> help "The name of the public key file"
            )

runGenerateVRFKeyPairCommand :: GenerateVRFKeyPairCommand -> IO ()
runGenerateVRFKeyPairCommand cmd = do
    print $ privateKeyName cmd <> " " <> publicKeyName cmd
