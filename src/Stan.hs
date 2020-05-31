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

import Colourista (errorMessage, formatWith, italic)
import HieTypes (HieFile (..))
import Trial (Trial (..), prettyPrintTaggedTrial, prettyPrintTrial, trialToMaybe)

import Stan.Analysis (Analysis (..), runAnalysis)
import Stan.Analysis.Pretty (prettyShowAnalysis)
import Stan.Cabal (createCabalExtensionsMap)
import Stan.Cli (InspectionArgs (..), StanArgs (..), StanCommand (..), TomlToCliArgs (..),
                 runStanCli)
import Stan.Config (ConfigP (..), applyConfig, configToCliCommand, defaultConfig, finaliseConfig)
import Stan.Core.Id (Id (..))
import Stan.EnvVars (EnvVars (..), getEnvVars)
import Stan.Hie (readHieFiles)
import Stan.Inspection (prettyShowInspection, prettyShowInspectionShort)
import Stan.Inspection.All (inspections, lookupInspectionById)
import Stan.Observation (prettyShowIgnoredObservations)
import Stan.Report (generateReport)
import Stan.Toml (getTomlConfig)
-- import Stan.Hie.Debug (debugHieFile)


run :: IO ()
run = runStanCli >>= \case
    Stan stanArgs -> runStan stanArgs
    StanInspection inspectionArgs -> runInspection inspectionArgs
    StanTomlToCli tomlToCliArgs -> runTomlToCli tomlToCliArgs

runStan :: StanArgs -> IO ()
runStan StanArgs{..} = do
    -- ENV vars
    EnvVars{..} <- getEnvVars
    let defConfTrial = envVarsUseDefaultConfigFile <> stanArgsUseDefaultConfigFile
    putTextLn $ prettyPrintTaggedTrial defConfTrial
    let useDefConfig = maybe True snd (trialToMaybe defConfTrial)
    -- config
    tomlConfig <- getTomlConfig useDefConfig stanArgsConfigFile
    let configTrial = finaliseConfig $ defaultConfig <> tomlConfig <> stanArgsConfig
    putTextLn $ prettyPrintTrial configTrial
    whenJust (trialToMaybe configTrial) $ \config -> do
        hieFiles <- readHieFiles stanArgsHiedir
        -- create cabal default extensions map
        cabalExtensionsMap <- createCabalExtensionsMap stanArgsCabalFilePath hieFiles
        -- get checks for each file
        let checksMap = applyConfig (map hie_hs_file hieFiles) config

        let analysis = runAnalysis cabalExtensionsMap checksMap (configObservations config) hieFiles
        -- show what observations are ignored
        putText $ prettyShowIgnoredObservations
            (configObservations config)
            (analysisIgnoredObservations analysis)
        -- show the result
        let res = prettyShowAnalysis analysis stanArgsReportSettings
        putTextLn res

        when stanArgsReport $
            putTextLn "Report is generated here -> stan.html" >> generateReport res
--    debugHieFile "target/Target/Infinite.hs" hieFiles

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
