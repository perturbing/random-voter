module Options (
    Options (..),
    options,
) where

import Commands (Command, commandParser)
import Data.Version (showVersion)
import Options.Applicative
import Paths_random_voter_cli (version)

newtype Options = Options
    { command :: Command
    }

options :: ParserInfo Options
options = info (helper <*> versionOption <*> optionsParser) description

optionsParser :: Parser Options
optionsParser = Options <$> commandParser

versionOption :: Parser (a -> a)
versionOption =
    infoOption
        ("random-voter-cli " <> showVersion version)
        (long "version" <> short 'v' <> help "Show version.")

description :: InfoMod Options
description =
    fullDesc
        <> progDesc "CLI for VRF key management and creating voting transactions."
        <> header "random-voter-cli: a CLI for VRF management transactions."
