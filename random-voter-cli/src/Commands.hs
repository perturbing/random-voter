{-# LANGUAGE LambdaCase #-}

module Commands (
    Command,
    commandParser,
    runCommand,
) where

import Commands.GenerateScript (GenerateScriptCommand, generateScriptCommandParser, runGenerateScriptCommand)
import Commands.GenerateVRFKeyPair (GenerateVRFKeyPairCommand, generateVRFKeyPairCommandParser, runGenerateVRFKeyPairCommand)
import Commands.GenerateVRFOutput (GenerateVRFOutputCommand, generateVRFOutputCommandParser, runGenerateVRFOutputCommand)
import Data.Foldable (fold)
import Options.Applicative (Parser, command, hsubparser, info, progDesc)

data Command
    = GenerateVRFKeyPair GenerateVRFKeyPairCommand
    | GenerateVRFOutput GenerateVRFOutputCommand
    | GenerateScript GenerateScriptCommand

-- Parsers

commandParser :: Parser Command
commandParser =
    hsubparser $
        fold
            [ command
                "generate-keypair"
                ( info
                    (GenerateVRFKeyPair <$> generateVRFKeyPairCommandParser)
                    (progDesc "Generate a VRF key pair")
                )
            , command
                "generate-vrf-output-redeemer"
                ( info
                    (GenerateVRFOutput <$> generateVRFOutputCommandParser)
                    (progDesc "Generate a VRF output and proof")
                )
            , command
                "generate-script"
                ( info
                    (GenerateScript <$> generateScriptCommandParser)
                    (progDesc "Generate the voter script credential given a vrf public key")
                )
            ]

-- Implementations

runCommand :: Command -> IO ()
runCommand = \case
    GenerateVRFKeyPair cmd -> runGenerateVRFKeyPairCommand cmd
    GenerateVRFOutput cmd -> runGenerateVRFOutputCommand cmd
    GenerateScript cmd -> runGenerateScriptCommand cmd
