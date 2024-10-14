{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Main running module.
-}

module Stan
    ( run
    , runStan
    , getAnalysis
    , getStanConfig

      -- ** Internal
    , createCabalExtensionsMap
    ) where

import Colourista (errorMessage, formatWith, infoMessage, italic, successMessage, warningMessage)
import Data.Aeson.Micro (encode)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath (takeFileName)
import Trial (Trial (..), prettyTaggedTrial, prettyTrial, prettyTrialWith, trialToMaybe,
              whenResult_)

import Stan.Analysis (Analysis (..), runAnalysis)
import Stan.Analysis.Pretty (prettyShowAnalysis)
import Stan.Browse (openBrowser)
import Stan.Cabal (createCabalExtensionsMap, usedCabalFiles)
import Stan.Cli (CliToTomlArgs (..), InspectionArgs (..), ReportArgs (..), StanArgs (..),
                 StanCommand (..), TomlToCliArgs (..), runStanCli)
import Stan.Config (ConfigP (..), applyConfig, configToCliCommand, defaultConfig, finaliseConfig, Config)
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
import qualified Slist as Slist
import qualified Data.Set as Set
import qualified Data.Text as T
import Language.Haskell.Exts
import Stan.FileInfo (FileInfo(..))


run :: IO ()
run = runStanCli >>= \case
    Stan stanArgs -> runStan stanArgs
    StanInspection inspectionArgs -> runInspection inspectionArgs
    StanTomlToCli tomlToCliArgs -> runTomlToCli tomlToCliArgs
    StanCliToToml cliToTomlArgs -> runCliToToml cliToTomlArgs
    StanInspectionsToMd -> putTextLn $ inspectionsMd inspections

getStanConfig :: StanArgs -> Bool -> IO (Trial Text Config, Bool, EnvVars)
getStanConfig StanArgs{..} notJson = do
    -- ENV vars
    env@EnvVars{..} <- getEnvVars
    let defConfTrial = envVarsUseDefaultConfigFile <> stanArgsUseDefaultConfigFile
    when notJson $ do
        infoMessage "Checking environment variables and CLI arguments for default configurations file usage..."
        putTextLn $ indent $ prettyTaggedTrial defConfTrial
    let useDefConfig = maybe True snd (trialToMaybe defConfTrial)
    -- config
    tomlConfig <- getTomlConfig notJson useDefConfig stanArgsConfigFile
    let configTrial = finaliseConfig $ defaultConfig <> tomlConfig <> stanArgsConfig
    when notJson $ do
        infoMessage "The following Configurations are used:\n"
        putTextLn $ indent $ prettyTrialWith (toString . prettyConfigCli) configTrial
    pure (configTrial, useDefConfig, env)

getAnalysis :: StanArgs -> Bool -> Config -> [HieFile] -> IO Analysis
getAnalysis StanArgs{..} notJson config hieFiles = do
    -- create cabal default extensions map
    cabalExtensionsMap <- createCabalExtensionsMap notJson stanArgsCabalFilePath hieFiles
    -- get checks for each file
    let checksMap = applyConfig (map hie_hs_file hieFiles) config

    let analysis = runAnalysis cabalExtensionsMap checksMap (configIgnored config) hieFiles
    -- show what observations are ignored
    pure analysis

isPlutusObservations :: Observation -> Bool
isPlutusObservations Observation{..} =
  -- observationInspectionId includes PLU-STAN
  "PLU-STAN" `T.isInfixOf` unId observationInspectionId

isOnchainObservations :: Set FilePath -> Observation -> Bool
isOnchainObservations files obs = Set.member (observationFile obs) files

isFileOnchainContract :: FilePath -> IO Bool
isFileOnchainContract file = do
  result <- parseFile file
  pure $ case result of
    ParseOk (Module _ _ _ _ decls) -> any isOnchainModuleAnn decls
    _otherwise -> False

isOnchainModuleAnn :: Decl SrcSpanInfo -> Bool
isOnchainModuleAnn (AnnPragma _ (ModuleAnn _ (Lit _ (String _ "onchain-contract" _)))) = True
isOnchainModuleAnn (AnnPragma _ (ModuleAnn _ (Paren _ (ExpTypeSig _ (Lit _ (String _ "onchain-contract" _)) _)))) = True
isOnchainModuleAnn _ = False

onchainFiles :: [HieFile] -> IO (Set FilePath)
onchainFiles hieFiles = do
  let files = map hie_hs_file hieFiles
  fromList <$> filterM isFileOnchainContract files

onchainCondition :: Set FilePath -> Observation -> Bool
onchainCondition contracts obs = not (isPlutusObservations obs) || isOnchainObservations contracts obs

filterForOnchain :: Set FilePath -> FileInfo -> FileInfo
filterForOnchain contracts info@FileInfo{..}= info {
  fileInfoObservations = Slist.filter (onchainCondition contracts) fileInfoObservations }

removeOffchain :: [HieFile] ->Analysis -> IO Analysis
removeOffchain hieFiles analysis = do
  contracts <- onchainFiles hieFiles
  pure analysis {
          analysisObservations = Slist.filter (onchainCondition contracts) (analysisObservations analysis)
        , analysisFileMap = fmap (filterForOnchain contracts) (analysisFileMap analysis)
        -- TODO: we might want to add those filtered observations to the ignored list
        -- but i'm not sure if it's a good idea
      }

runStan :: StanArgs -> IO ()
runStan stanArgs@StanArgs{..} = do
    let notJson = not stanArgsJsonOut
    (configTrial, useDefConfig, env) <- getStanConfig stanArgs notJson
    whenResult_ configTrial $ \warnings config -> do
        hieFiles <- readHieFiles stanArgsHiedir
        --NOTE: this filter is applied only for CLI, hls will still show all observations
        analysis <- getAnalysis stanArgs notJson config hieFiles >>= removeOffchain hieFiles
        -- show what observations are ignored
        when notJson $ putText $ indent $ prettyShowIgnoredObservations
            (configIgnored config)
            (analysisIgnoredObservations analysis)
        -- show the result
        let observations = analysisObservations analysis
        let isNullObs = null observations
        if notJson
        then do
            if isNullObs
            then successMessage "All clean! Stan did not find any observations at the moment."
            else warningMessage "Stan found the following observations for the project:\n"
            putTextLn $ prettyShowAnalysis analysis stanArgsOutputSettings
        else putLBSLn $ encode analysis

        -- report generation
        whenJust stanArgsReport $ \ReportArgs{..} -> do
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
            when notJson $ infoMessage "Report is generated here -> stan.html"
            when reportArgsBrowse $ openBrowser "stan.html"

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
    partialConfig <- getTomlConfig True useDefConfig tomlToCliArgsFilePath
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
