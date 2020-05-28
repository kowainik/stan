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

import Relude.Extra.Tuple (mapToSnd)

import Colourista (errorMessage, formatWith, infoMessage, italic, warningMessage)
import Control.Exception (catch)
import Extensions (CabalException, ExtensionsError (..), ParsedExtensions, parseCabalFileExtensions)
import HieTypes (HieFile (..))
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, listDirectory)
import System.FilePath (takeExtension, (</>))
import System.IO.Unsafe (unsafeInterleaveIO)
import Trial (Trial (..), prettyPrintTaggedTrial, prettyPrintTrial, trialToMaybe)

import Stan.Analysis (runAnalysis)
import Stan.Analysis.Pretty (prettyShowAnalysis)
import Stan.Cli (InspectionArgs (..), StanArgs (..), StanCommand (..), TomlToCliArgs (..),
                 runStanCli)
import Stan.Config (configToCliCommand, defaultConfig, finaliseConfig)
import Stan.Core.Id (Id (..))
import Stan.EnvVars (EnvVars (..), getEnvVars)
import Stan.Hie (readHieFiles)
import Stan.Inspection (prettyShowInspection, prettyShowInspectionShort)
import Stan.Inspection.All (inspections, lookupInspectionById)
import Stan.Toml (getTomlConfig)
-- import Stan.Hie.Debug (debugHieFile)

import qualified Data.Map.Strict as Map


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
    let config = finaliseConfig $ defaultConfig <> tomlConfig <> stanArgsConfig
    putTextLn $ prettyPrintTrial config

    hieFiles <- readHieFiles stanArgsHiedir
    -- create cabal default extensions map
    cabalExtensionsMap <- createCabalExtensionsMap stanArgsCabalFilePath hieFiles

    let analysis = runAnalysis cabalExtensionsMap hieFiles
    putTextLn $ prettyShowAnalysis analysis stanArgsReportSettings
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
            errorMessage $ "Could not get Configurations:"
            putTextLn $ prettyPrintTrial fiasco

{- | From a given path to cabal file and 'HieFile's create the map from modules
(that are in .cabal file) to the resulting parsed extensions for each.
-}
createCabalExtensionsMap
    :: [FilePath]
    -> [HieFile]
    -> IO (Map FilePath (Either ExtensionsError ParsedExtensions))
createCabalExtensionsMap cabalPath hies = case cabalPath of
    -- if cabal files are not specified via CLI option
    -- try to find cabal files in current directory
    [] -> findCabalFiles >>= \case
        -- if cabal file is not found, pass the empty map instead
        [] -> do
            warningMessage ".cabal file not found in the current directory."
            infoMessage " 💡 Try using --cabal-file-path option to specify the path to the .cabal file.\n"
            pure mempty
        -- else concat map for each @.cabal@ file.
        cabals -> fmap mconcat $ mapM getExtensionsWithCabal cabals
    -- if cabal file specified via CLI option
    cabals -> fmap mconcat $ forM (ordNub cabals) $ \cabal ->
        ifM (doesFileExist cabal)
        {- then -} (getExtensionsWithCabal cabal)
        {- else -} (errorMessage (".cabal file does not exist: " <> toText cabal) >> exitFailure)
  where
    getExtensionsWithCabal
        :: FilePath
        -> IO (Map FilePath (Either ExtensionsError ParsedExtensions))
    getExtensionsWithCabal cabal = do
        infoMessage $ "Using the following .cabal file: " <> toText cabal <> "\n"
        (Right <<$>> parseCabalFileExtensions cabal)
            `catch` handleCabalErr
      where
        handleCabalErr
            :: CabalException
            -> IO (Map FilePath (Either ExtensionsError ParsedExtensions))
        handleCabalErr err = do
            errorMessage "Error when parsing cabal file. Stan will continue without information from .cabal file"
            pure $ Map.fromList $
                map (mapToSnd (const $ Left $ CabalError err) . hie_hs_file) hies

{- | Recursively find all @.cabal@ files in the current directory and its
subdirectories. It returns maximum 1 @.cabal@ file from each directory.
-}
findCabalFiles :: IO [FilePath]
findCabalFiles = do
    dir <- getCurrentDirectory
    curDirCabal <- findCabalFileDir dir
    dirs <- getSubdirsRecursive dir
    subDirsCabals <- mapM findCabalFileDir dirs
    pure $ catMaybes $ curDirCabal : subDirsCabals

-- | Find a @.cabal@ file in the given directory.
-- TODO: better error handling in stan.
findCabalFileDir :: FilePath -> IO (Maybe FilePath)
findCabalFileDir dir = do
    dirContent <- listDirectory dir
    let cabalFiles = filter isCabal dirContent
    pure $ case cabalFiles of
        []          -> Nothing -- throwError $ NoCabalFile dirPath
        [cabalFile] -> Just $ dir </> cabalFile
        x:_xs       -> Just x -- throwError $ MultipleCabalFiles (x :| xs)
  where
    isCabal :: FilePath -> Bool
    isCabal p = takeExtension p == ".cabal"

getSubdirsRecursive :: FilePath -> IO [FilePath]
getSubdirsRecursive fp = do
    all' <- filter nonGenDir <$> listDirectory fp
    dirs <- filterM doesDirectoryExist (mkRel <$> all')
    case dirs of
        [] -> pure []
        ds -> do
            next <- unsafeInterleaveIO $ foldMapA getSubdirsRecursive ds
            pure $ dirs ++ next
  where
    nonGenDir :: FilePath -> Bool
    nonGenDir d =
           d /= "dist"
        && d /= "dist-newstyle"
        && d /= ".stack-work"

    mkRel :: FilePath -> FilePath
    mkRel = (fp </>)
