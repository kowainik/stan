{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

CLI commands and options for @stan@.
-}

module Stan.Cli
    ( StanCommand (..)
    , StanArgs (..)
    , InspectionArgs (..)
    , runStanCli
    ) where

import Colourista (blue, bold, formatWith)
import Data.Version (showVersion)
import Development.GitRev (gitCommitDate, gitHash)
import Options.Applicative (Parser, ParserInfo, command, execParser, flag, fullDesc, help, helper,
                            hsubparser, info, infoOption, long, metavar, progDesc, short,
                            showDefault, strArgument, strOption, value)

import Stan.Core.Id (Id (..))
import Stan.Core.Toggle (ToggleSolution (..))
import Stan.Inspection (Inspection)

import qualified Paths_stan as Meta (version)


-- | Commands used in Stan CLI.
data StanCommand
    = Stan StanArgs  -- ^ Just @stan@ with its options.
    | StanInspection InspectionArgs  -- ^ @stan inspection@.

-- | Options used for the main @stan@ command.
data StanArgs = StanArgs
    { stanArgsHiedir         :: !FilePath  -- ^ Directory with HIE files
    , stanArgsToggleSolution :: !ToggleSolution  -- ^ Hide 'inspectionSolution'.
    }

-- | Options used for the @stan inspection@ command.
newtype InspectionArgs = InspectionArgs
    { inspectionArgsId :: Maybe (Id Inspection)
    }

-- | Run main parser of the @stan@ command line tool.
-- TODO: use customExecParser
runStanCli :: IO StanCommand
runStanCli = execParser stanCliParser

stanCliParser :: ParserInfo StanCommand
stanCliParser = info (helper <*> versionP <*> stan) $
    fullDesc <> progDesc "Haskell Static Analyser"

{- | Stan tool parser. It either uses the named commands or the main @stan@
command.
-}
stan :: Parser StanCommand
stan = stanInspectionP <|> stanP

-- | @stan@ command parser.
stanP :: Parser StanCommand
stanP = do
    stanArgsHiedir <- hiedirP
    stanArgsToggleSolution <- toggleSolutionP
    pure $ Stan StanArgs{..}

-- | @stan inspection@ command parser.
stanInspectionP :: Parser StanCommand
stanInspectionP = hsubparser $
    command "inspection" (info inspectionP (progDesc "Show all Inspections"))
  where
    inspectionP :: Parser StanCommand
    inspectionP = do
        inspectionArgsId <- Id <<$>> optional ( strArgument
            (metavar "INSPECTION_ID" <> help "Show specific Inspection information"))
        pure $ StanInspection InspectionArgs{..}

hiedirP :: Parser FilePath
hiedirP = strOption $ mconcat
    [ long "hiedir"
    , metavar "FILE_PATH"
    , value ".hie"
    , showDefault
    , help "Relative path to the directory with HIE files"
    ]

-- | The solution is shown by default and gets hidden when option is specified.
toggleSolutionP :: Parser ToggleSolution
toggleSolutionP = flag ShowSolution HideSolution $ mconcat
    [ long "hide-solution"
    , help "Hide verbose solution information for observations"
    ]


-- | Show the version of the tool.
versionP :: Parser (a -> a)
versionP = infoOption stanVersion
    $ long "version"
   <> short 'v'
   <> help "Show Stan's version"

stanVersion :: String
stanVersion = toString $ intercalate "\n"
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
