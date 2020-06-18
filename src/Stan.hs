{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Main running module.
-}

module Stan
    ( run

      -- ** Internal
    , createCabalExtensionsMap
    ) where

import Colourista (errorMessage, formatWith, infoMessage, italic, successMessage, warningMessage)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Environment (getArgs)
import System.FilePath (takeFileName)
import Trial (Trial (..), prettyTaggedTrial, prettyTrial, prettyTrialWith, trialToMaybe,
              whenResult_)

import Stan.Analysis (Analysis (..), runAnalysis)
import Stan.Analysis.Pretty (prettyShowAnalysis)
import Stan.Cabal (createCabalExtensionsMap, usedCabalFiles)
import Stan.Cli (CliToTomlArgs (..), InspectionArgs (..), StanArgs (..), StanCommand (..),
                 TomlToCliArgs (..), runStanCli)
import Stan.Config (ConfigP (..), applyConfig, configToCliCommand, defaultConfig, finaliseConfig)
import Stan.Config.Pretty (prettyConfigCli)
import Stan.Core.Id (Id (..))
import Stan.EnvVars (EnvVars (..), envVarsToText, getEnvVars)
import Stan.Hie (readHieFiles)
import Stan.Hie.Compat (HieFile (..))
import Stan.Info (ProjectInfo (..), StanEnv (..))
import Stan.Inspection (Inspection (..), inspectionsMd, prettyShowInspection,
                        prettyShowInspectionShort)
import Stan.Inspection.All (getInspectionById, inspections, lookupInspectionById)
import Stan.Observation (Observation (..), prettyShowIgnoredObservations)
import Stan.Report (generateReport)
import Stan.Severity (Severity (Error))
import Stan.Toml (configCodec, getTomlConfig, usedTomlFiles)

import qualified Toml


run :: IO ()
run = runStanCli >>= \case
    Stan stanArgs -> runStan stanArgs
    StanInspection inspectionArgs -> runInspection inspectionArgs
    StanTomlToCli tomlToCliArgs -> runTomlToCli tomlToCliArgs
    StanCliToToml cliToTomlArgs -> runCliToToml cliToTomlArgs
    StanInspectionsToMd -> putTextLn $ inspectionsMd inspections

runStan :: StanArgs -> IO ()
runStan StanArgs{..} = do
    -- ENV vars
    env@EnvVars{..} <- getEnvVars
    let defConfTrial = envVarsUseDefaultConfigFile <> stanArgsUseDefaultConfigFile
    infoMessage "Checking environment variables and CLI arguments for default configurations file usage..."
    putTextLn $ indent $ prettyTaggedTrial defConfTrial
    let useDefConfig = maybe True snd (trialToMaybe defConfTrial)
    -- config
    tomlConfig <- getTomlConfig useDefConfig stanArgsConfigFile
    let configTrial = finaliseConfig $ defaultConfig <> tomlConfig <> stanArgsConfig
    infoMessage "The following Configurations are used:\n"
    putTextLn $ indent $ prettyTrialWith (toString . prettyConfigCli) configTrial
    whenResult_ configTrial $ \warnings config -> do
        hieFiles <- readHieFiles stanArgsHiedir
        -- create cabal default extensions map
        cabalExtensionsMap <- createCabalExtensionsMap stanArgsCabalFilePath hieFiles
        -- get checks for each file
        let checksMap = applyConfig (map hie_hs_file hieFiles) config

        let analysis = runAnalysis cabalExtensionsMap checksMap (configIgnored config) hieFiles
        -- show what observations are ignored
        putText $ indent $ prettyShowIgnoredObservations
            (configIgnored config)
            (analysisIgnoredObservations analysis)
        -- show the result
        let observations = analysisObservations analysis
        let isNullObs = null observations
        if isNullObs
        then successMessage "All clean! Stan did not find any observations at the moment."
        else do
            warningMessage "Stan found the following observations for the project:\n"
        putTextLn $ prettyShowAnalysis analysis stanArgsReportSettings

        -- report generation
        when stanArgsReport $ do
            -- Project Info
            piName <- takeFileName <$> getCurrentDirectory
            piCabalFiles <- usedCabalFiles stanArgsCabalFilePath
            let piHieDir = stanArgsHiedir
            let piFileNumber = length hieFiles
            -- Stan Env Info
            seCliArgs <- getArgs
            seTomlFiles <- usedTomlFiles useDefConfig stanArgsConfigFile
            let stanEnv = StanEnv
                    { seEnvVars = envVarsToText env
                    , ..
                    }
            generateReport analysis config warnings stanEnv ProjectInfo{..}
            infoMessage "Report is generated here -> stan.html"

        -- decide on exit status
        when
            (  not isNullObs
            && any ((>= Error) . getObservationSeverity) observations
            )
            exitFailure
  where
    getObservationSeverity :: Observation -> Severity
    getObservationSeverity = inspectionSeverity . getInspectionById . observationInspectionId

    indent :: Text -> Text
    indent = unlines . map ("    " <>) . lines

runInspection :: InspectionArgs -> IO ()
runInspection InspectionArgs{..} = case inspectionArgsId of
    Nothing  -> for_ inspections (putTextLn . prettyShowInspectionShort)
    Just insId -> case lookupInspectionById insId of
        Just ins -> putTextLn $ prettyShowInspection ins
        Nothing  -> do
            errorMessage $ "Inspection with such ID does not exist: " <> unId insId
            putTextLn $ " 💡 " <> formatWith [italic] "Use 'stan inspection' to see the list of all available inspections."

runTomlToCli :: TomlToCliArgs -> IO ()
runTomlToCli TomlToCliArgs{..} = do
    let useDefConfig = isNothing tomlToCliArgsFilePath
    partialConfig <- getTomlConfig useDefConfig tomlToCliArgsFilePath
    case finaliseConfig partialConfig of
        Result _ res -> putTextLn $ configToCliCommand res
        fiasco -> do
            errorMessage "Could not get Configurations:"
            putTextLn $ prettyTrial fiasco

runCliToToml :: CliToTomlArgs -> IO ()
runCliToToml CliToTomlArgs{..} = do
    let toml = Toml.encode configCodec cliToTomlArgsConfig
    case cliToTomlArgsFilePath of
        Nothing -> do
            putTextLn toml
            infoMessage "Copy-paste the above TOML into .stan.toml and stan will pick up this file on the next run"
        Just path -> do
            isFile <- doesFileExist path
            if isFile
            then errorMessage $ "Aborting writing to file because it already exists: " <> toText path
            else do
                writeFileText path toml
                infoMessage $ "TOML configuration is written to file: " <> toText path
