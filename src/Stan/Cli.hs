{-# LANGUAGE ApplicativeDo #-}

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
    , TomlToCliArgs (..)
    , CliToTomlArgs (..)
    , runStanCli
    , stanParserPrefs
    , stanCliParser
    ) where

import Colourista (blue, bold, formatWith, reset, yellow)
import Colourista.Short (b)
import Data.Char (toUpper)
import Options.Applicative (CommandFields, Mod, Parser, ParserInfo (..), ParserPrefs, auto, columns,
                            command, commandGroup, customExecParser, flag, flag', fullDesc, help,
                            helpLongEquals, helper, hidden, hsubparser, info, infoOption, internal,
                            long, metavar, multiSuffix, option, prefs, progDesc, short,
                            showDefaultWith, showHelpOnEmpty, showHelpOnError, strArgument,
                            strOption, subparserInline, value)
import Options.Applicative.Help.Chunk (stringChunk)
import Trial (TaggedTrial, fiascoOnEmpty)
import Trial.OptparseApplicative (taggedTrialParser)

import Stan.Category (Category (..))
import Stan.Config (Check (..), CheckFilter (..), CheckType (..), ConfigP (..), PartialConfig,
                    Scope (..))
import Stan.Core.Id (Id (..))
import Stan.Info (prettyStanVersion, stanVersion)
import Stan.Inspection (Inspection)
import Stan.Observation (Observation)
import Stan.Report.Settings (ReportSettings (..), ToggleSolution (..), Verbosity (..))


-- | Commands used in Stan CLI.
data StanCommand
    = Stan !StanArgs  -- ^ Just @stan@ with its options.
    | StanInspection !InspectionArgs  -- ^ @stan inspection@.
    | StanTomlToCli !TomlToCliArgs  -- ^ @stan toml-to-cli@
    | StanCliToToml !CliToTomlArgs  -- ^ @stan cli-to-toml@
    | StanInspectionsToMd  -- ^ @stan inspections-to-md@

-- | Options used for the main @stan@ command.
data StanArgs = StanArgs
    { stanArgsHiedir               :: !FilePath  -- ^ Directory with HIE files
    , stanArgsCabalFilePath        :: ![FilePath]  -- ^ Path to @.cabal@ files.
    , stanArgsReportSettings       :: !ReportSettings  -- ^ Settings for report
    , stanArgsReport               :: !Bool  -- ^ Create @HTML@ report?
    , stanArgsUseDefaultConfigFile :: !(TaggedTrial Text Bool)  -- ^ Use default @.stan.toml@ file
    , stanArgsConfigFile           :: !(Maybe FilePath)  -- ^ Path to a custom configurations file.
    , stanArgsConfig               :: !PartialConfig
    }

-- | Options used for the @stan inspection@ command.
newtype InspectionArgs = InspectionArgs
    { inspectionArgsId :: Maybe (Id Inspection)
    }

-- | Options used for the @stan toml-to-cli@ command.
newtype TomlToCliArgs = TomlToCliArgs
    { tomlToCliArgsFilePath :: Maybe FilePath
    }

-- | Options used for the @stan cli-to-toml@ command.
data CliToTomlArgs = CliToTomlArgs
    { cliToTomlArgsFilePath :: !(Maybe FilePath)
    , cliToTomlArgsConfig   :: !PartialConfig
    }

-- | Run main parser of the @stan@ command line tool.
runStanCli :: IO StanCommand
runStanCli = customExecParser stanParserPrefs stanCliParser

-- | To turn on some special options.
stanParserPrefs :: ParserPrefs
stanParserPrefs = prefs $ mconcat
    [ helpLongEquals
    , showHelpOnEmpty
    , showHelpOnError
    , subparserInline
    , multiSuffix "s"
    , columns 100
    ]

stanCliParser :: ParserInfo StanCommand
stanCliParser = modifyHeader $ info (helper <*> versionP <*> stan) fullDesc

{- | Stan tool parser. It either uses the named commands or the main @stan@
command.
-}
stan :: Parser StanCommand
stan =  stanInspectionP
    <|> stanTomlToCliP
    <|> stanCliToTomlP
    <|> stanInspectionsToMd
    <|> stanP

-- | @stan@ command parser.
stanP :: Parser StanCommand
stanP = do
    stanArgsConfig <- configP
    stanArgsReport <- reportP
    stanArgsHiedir <- hiedirP
    stanArgsCabalFilePath <- cabalFilePathP
    stanArgsConfigFile <- configFileP
    stanArgsUseDefaultConfigFile <- useDefaultConfigFileP
    stanArgsReportSettings <- reportSettingsP
    pure $ Stan StanArgs{..}

-- | @stan inspection@ command parser.
stanInspectionP :: Parser StanCommand
stanInspectionP = hsubparser $
    command "inspection" (info inspectionP (progDesc "Show all Inspections"))
    <> commandVar "INSPECTION"
    <> help "Command to show all or specific inspection"
  where
    inspectionP :: Parser StanCommand
    inspectionP = do
        inspectionArgsId <- Id <<$>> optional ( strArgument
            (metavar "INSPECTION_ID" <> help "Show specific Inspection information"))
        pure $ StanInspection InspectionArgs{..}

stanTomlToCliP :: Parser StanCommand
stanTomlToCliP = hsubparser $ commandGroup "TOML Configurations"
    <> command "toml-to-cli"
        ( info tomlToCliP
            (progDesc "Convert TOML configuration file into stan CLI command")
        )
    <> commandVar "TOML-TO-CLI"
    <> help "Command to convert TOML configurations to CLI"
  where
    tomlToCliP :: Parser StanCommand
    tomlToCliP = do
        tomlToCliArgsFilePath <- configFileP
        pure $ StanTomlToCli TomlToCliArgs{..}

stanCliToTomlP :: Parser StanCommand
stanCliToTomlP = hsubparser $ commandGroup "TOML Configurations"
    <> command "cli-to-toml"
        ( info cliToTomlP
            (progDesc "Convert CLI arguments into stan TOML configuration")
        )
    <> commandVar "CLI-TO-TOML"
    <> help "Command to convert CLI configurations to TOML"
  where
    cliToTomlP :: Parser StanCommand
    cliToTomlP = do
        cliToTomlArgsFilePath <- configFileP
        cliToTomlArgsConfig   <- configP
        pure $ StanCliToToml CliToTomlArgs{..}

stanInspectionsToMd :: Parser StanCommand
stanInspectionsToMd = hsubparser
    $  command "inspections-to-md"
        (info (pure StanInspectionsToMd)
             (progDesc "Create md with all inspections info")
        )
    <> help "Create md with all inspections info"
    <> hidden <> internal

hiedirP :: Parser FilePath
hiedirP = strOption $ mconcat
    [ long "hiedir"
    , metavar "DIR_PATH"
    , value ".hie"
    , showDefaultWith (formatWith [blue, bold])
    , help "Relative path to the directory with HIE files"
    ]

cabalFilePathP :: Parser [FilePath]
cabalFilePathP = many $ strOption $ mconcat
    [ long "cabal-file-path"
    , metavar "FILE_PATH"
    , help "Relative path to the .cabal file (can specify many of this option)"
    ]

configFileP :: Parser (Maybe FilePath)
configFileP = optional $ strOption $ mconcat
    [ long "config-file"
    , metavar "FILE_PATH"
    , help "Relative path to the .toml configurations file"
    ]

useDefaultConfigFileP :: Parser (TaggedTrial Text Bool)
useDefaultConfigFileP = taggedTrialParser "no-default" $ flag' False $ mconcat
    [ long "no-default"
    , help "Ignore local .stan.toml configuration file"
    ]

reportP :: Parser Bool
reportP = do
    res <- optional $ hsubparser $
        command "report"
            (info pass (progDesc "Generate HTML Report"))
        <> commandGroup "Reporting"
        <> commandVar "REPORT"
        <> help "Command to generate an HTML Report"
    pure $ isJust res

reportSettingsP :: Parser ReportSettings
reportSettingsP = do
    reportSettingsVerbosity <- verbosityP
    reportSettingsSolutionVerbosity <- toggleSolutionP
    pure ReportSettings{..}

-- | The solution is shown by default and gets hidden when option is specified.
toggleSolutionP :: Parser ToggleSolution
toggleSolutionP = flag ShowSolution HideSolution $ mconcat
    [ long "hide-solution"
    , help "Hide verbose solution information for observations"
    ]

-- | The 'Observation' is shown juicy by default and gets shortened when option is specified.
verbosityP :: Parser Verbosity
verbosityP = flag Verbose NonVerbose $ mconcat
    [ long "short"
    , short 's'
    , help "Hide verbose output information for observations"
    ]

data ConfigCommand
    = CheckCommand !Check
    | RemoveCommand !Scope
    | IgnoreCommand !(Id Observation)

partitionCommands :: [ConfigCommand] -> ([Check], [Scope], [Id Observation])
partitionCommands [] = ([], [], [])
partitionCommands (cmd : rest) =
    let (check, remove, obs) = partitionCommands rest
    in case cmd of
        CheckCommand ch -> (ch:check, remove, obs)
        RemoveCommand r -> (check, r:remove, obs)
        IgnoreCommand o -> (check, remove, o:obs)

configP :: Parser PartialConfig
configP = do
    res <- many $
        cmd "check" "Specify list of checks" CheckCommand checkP
        <|> cmd "remove" "Specify scope to be removed" RemoveCommand scopeP
        <|> cmd "ignore" "Specify list of what needs to be ignored" IgnoreCommand (idP "Observations")
    pure $
        let (checks, removed, ignored) = partitionCommands res
        in ConfigP
            { configChecks  = fiascoOnEmpty "CLI" "checks" checks
            , configRemoved = fiascoOnEmpty "CLI" "remove" removed
            , configIgnored = fiascoOnEmpty "CLI" "ignore" ignored
            }
  where
    cmd :: String -> String -> (a -> ConfigCommand) -> Parser a -> Parser ConfigCommand
    cmd name h cc p = hsubparser
        ( command name
            (info (cc <$> p) (progDesc h))
        <> commandVar (map toUpper name)
        <> help ("Command to " <> h)
        <> commandGroup "CLI Configurations"
        )

-- | Parser of an 'Id'. Receives a string to specify in Help what kind of ID is this.
idP :: String -> Parser (Id a)
idP name = Id <$> strOption
    (long "id"
    <> metavar (map toUpper name <> "_ID")
    <> help (name <> " ID to be used"))

checkP :: Parser Check
checkP = do
    checkType <- checkTypeP
    checkFilter <- checkFilterP
    checkScope  <- scopeP
    pure Check{..}

checkTypeP :: Parser CheckType
checkTypeP =
    -- QUESTION: is it better than --type=Exclude or --type=Include
        flag' Include (long "include" <> help "Include check")
    <|> flag' Exclude (long "exclude" <> help "Exclude check")

checkFilterP :: Parser CheckFilter
checkFilterP =
        CheckInspection <$> idP "Inspection"
    <|> CheckSeverity <$> option auto
        -- TODO: how to specify all possible values here in help?
        (long "severity"
        <> metavar "SEVERITY"
        <> help "Inspection Severity to exclude or include")
    <|> CheckCategory . Category <$> strOption
        (long "category"
        <> metavar "CATEGORY"
        <> help "Inspection Category to exclude or include")
    <|> flag' CheckAll
        (long "filter-all"
        <> help "Exclude or include ALL inspections")

scopeP :: Parser Scope
scopeP =
        ScopeFile <$> strOption
        (long "file"
        <> metavar "FILE_PATH"
        <> help "File to exclude or include")
    <|> ScopeDirectory <$> strOption
        (long "directory"
        <> metavar "DIRECTORY_PATH"
        <> help "Directory to exclude or include")
    <|> flag' ScopeAll
        (long "scope-all"
        <> help "Apply check to all files")

-- | Show the version of the tool.
versionP :: Parser (a -> a)
versionP = infoOption (prettyStanVersion stanVersion)
    $ long "version"
    <> short 'v'
    <> help "Show Stan's version"
    <> hidden

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

commandVar :: String -> Mod CommandFields a
commandVar = metavar . b
