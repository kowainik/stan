{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

CLI commands and options for @stan@.
-}

module Stan.Cli
    ( CliArgs (..)
    , runStanCli
    ) where

import Colourista (blue, bold, formatWith)
import Data.Version (showVersion)
import Development.GitRev (gitCommitDate, gitHash)
import Options.Applicative (Parser, ParserInfo, execParser, fullDesc, help, helper, info,
                            infoOption, long, long, metavar, progDesc, short, showDefault,
                            strOption, value)

import qualified Paths_stan as Meta (version)


newtype CliArgs = CliArgs
    { cliArgsHiedir :: FilePath  -- ^ Directory with HIE files
    }


-- | Run main parser of the @stan@ command line tool.
runStanCli :: IO CliArgs
runStanCli = execParser stanCliParser

stanCliParser :: ParserInfo CliArgs
stanCliParser = info (helper <*> versionP <*> stanP) $
    fullDesc <> progDesc "Haskell Static Analyser"

stanP :: Parser CliArgs
stanP = do
    cliArgsHiedir <- hiedirP
    pure CliArgs{..}

hiedirP :: Parser FilePath
hiedirP = strOption $ mconcat
    [ long "hiedir"
    , metavar "FILE_PATH"
    , value ".hie"
    , showDefault
    , help "Relative path to the directory with HIE files"
    ]

-- | Show the version of the tool.
versionP :: Parser (a -> a)
versionP = infoOption policemanVersion
    $ long "version"
   <> short 'v'
   <> help "Show Stan's version"

policemanVersion :: String
policemanVersion = toString $ intercalate "\n"
    [ sVersion
    , sHash
    , sDate
    ]
  where
    fmt :: String -> String
    fmt = formatWith [blue, bold]

    sVersion, sHash, sDate :: String
    sVersion = fmt $ "Stan " <> "v" <> showVersion Meta.version
    sHash = " ➤ " <> fmt "Git revision: " <> $(gitHash)
    sDate = " ➤ " <> fmt "Commit date:  " <> $(gitCommitDate)
