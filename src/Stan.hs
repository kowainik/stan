{-# LANGUAGE PatternSynonyms #-}

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

import Colourista (errorMessage, formatWith, infoMessage, italic)
import HieTypes (HieFile (..))
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.FilePath (takeFileName)
import Trial (pattern FiascoL, pattern ResultL, Trial (..), prettyPrintTaggedTrial,
              prettyPrintTrial, trialToMaybe)

import Stan.Analysis (Analysis (..), runAnalysis)
import Stan.Analysis.Pretty (prettyShowAnalysis)
import Stan.Cabal (createCabalExtensionsMap, usedCabalFiles)
import Stan.Cli (CliToTomlArgs (..), InspectionArgs (..), StanArgs (..), StanCommand (..),
                 TomlToCliArgs (..), runStanCli)
import Stan.Config (ConfigP (..), applyConfig, configToCliCommand, defaultConfig, finaliseConfig)
import Stan.Core.Id (Id (..))
import Stan.EnvVars (EnvVars (..), envVarsToText, getEnvVars)
import Stan.Hie (readHieFiles)
import Stan.Info (ProjectInfo (..), StanEnv (..))
import Stan.Inspection (prettyShowInspection, prettyShowInspectionShort)
import Stan.Inspection.All (inspections, lookupInspectionById)
import Stan.Observation (prettyShowIgnoredObservations)
import Stan.Report (generateReport)
import Stan.Toml (configCodec, getTomlConfig, usedTomlFiles)

import qualified Toml


run :: IO ()
run = runStanCli >>= \case
    Stan stanArgs -> runStan stanArgs
    StanInspection inspectionArgs -> runInspection inspectionArgs
    StanTomlToCli tomlToCliArgs -> runTomlToCli tomlToCliArgs
    StanCliToToml cliToTomlArgs -> runCliToToml cliToTomlArgs

runStan :: StanArgs -> IO ()
runStan StanArgs{..} = do
    -- ENV vars
    env@EnvVars{..} <- getEnvVars
    let defConfTrial = envVarsUseDefaultConfigFile <> stanArgsUseDefaultConfigFile
    putTextLn $ prettyPrintTaggedTrial defConfTrial
    let useDefConfig = maybe True snd (trialToMaybe defConfTrial)
    -- config
    tomlConfig <- getTomlConfig useDefConfig stanArgsConfigFile
    let configTrial = finaliseConfig $ defaultConfig <> tomlConfig <> stanArgsConfig
    putTextLn $ prettyPrintTrial configTrial
    whenResult configTrial $ \warnings config -> do
        hieFiles <- readHieFiles stanArgsHiedir
        -- create cabal default extensions map
        cabalExtensionsMap <- createCabalExtensionsMap stanArgsCabalFilePath hieFiles
        -- get checks for each file
        let checksMap = applyConfig (map hie_hs_file hieFiles) config

        let analysis = runAnalysis cabalExtensionsMap checksMap (configIgnored config) hieFiles
        -- show what observations are ignored
        putText $ prettyShowIgnoredObservations
            (configIgnored config)
            (analysisIgnoredObservations analysis)
        -- show the result
        let res = prettyShowAnalysis analysis stanArgsReportSettings
        putTextLn res

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
  where
    whenResult :: Trial e a -> ([e] -> a -> IO ()) -> IO ()
    whenResult (FiascoL _) _      = pass
    whenResult (ResultL es a) act = act es a

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
            putTextLn $ prettyPrintTrial fiasco

runCliToToml :: CliToTomlArgs -> IO ()
runCliToToml CliToTomlArgs{..} = do
    let toml = Toml.encode configCodec cliToTomlArgsConfig
    case cliToTomlArgsFilePath of
        Nothing   -> putTextLn toml
        Just path -> do
            infoMessage $ "TOML configuration is written to file: " <> toText path
            writeFileText path toml
