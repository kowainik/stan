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

import Colourista (blue, bold, formatWith, reset, yellow)
import Data.Version (showVersion)
import Development.GitRev (gitCommitDate, gitHash)
import Options.Applicative (Parser, ParserInfo (..), ParserPrefs, command, customExecParser, flag,
                            fullDesc, help, helpLongEquals, helper, hsubparser, info, infoOption,
                            long, metavar, prefs, progDesc, short, showDefaultWith, showHelpOnEmpty,
                            strArgument, strOption, subparserInline, value)
import Options.Applicative.Help.Chunk (stringChunk)

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
    , stanArgsCabalFilePath  :: !(Maybe FilePath)  -- ^ Path to @.cabal@ file.
    , stanArgsToggleSolution :: !ToggleSolution  -- ^ Hide 'inspectionSolution'.
    }

-- | Options used for the @stan inspection@ command.
newtype InspectionArgs = InspectionArgs
    { inspectionArgsId :: Maybe (Id Inspection)
    }

-- | Run main parser of the @stan@ command line tool.
runStanCli :: IO StanCommand
runStanCli = customExecParser stanParserPrefs stanCliParser
  where
    -- To turn on some special options.
    stanParserPrefs :: ParserPrefs
    stanParserPrefs = prefs $ mconcat
        [ helpLongEquals
        , showHelpOnEmpty
        , subparserInline
        ]

stanCliParser :: ParserInfo StanCommand
stanCliParser = modifyHeader $ info (helper <*> versionP <*> stan) $
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
    stanArgsCabalFilePath <- cabalFilePathP
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
    , metavar "DIR_PATH"
    , value ".hie"
    , showDefaultWith (formatWith [blue, bold])
    , help "Relative path to the directory with HIE files"
    ]

cabalFilePathP :: Parser (Maybe FilePath)
cabalFilePathP = optional $ strOption $ mconcat
    [ long "cabal-file-path"
    , metavar "FILE_PATH"
    , help "Relative path to the .cabal file"
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

-- to put custom header which doesn't cut all spaces
modifyHeader :: ParserInfo a -> ParserInfo a
modifyHeader p = p { infoHeader = stringChunk $ toString header }

header :: Text
header = unlines
    [ yellow
    , "     ______________    _   __"
    , "    / ___/_  __/   |  / | / /"
    , "    \\__ \\ / / / /| | /  |/ / "
    , "   ___/ // / / ___ |/ /|  /  "
    , "  /____//_/ /_/  |_/_/ |_/   "
    , reset
    , "  Haskell " <> b "ST" <> "atic " <> b "AN" <> "alyser"
    ]
  where
    b :: Text -> Text
    b = formatWith [bold]
