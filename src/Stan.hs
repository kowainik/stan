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
import System.Directory (doesFileExist, getCurrentDirectory, listDirectory)
import System.FilePath (takeExtension, (</>))

import Stan.Analysis (runAnalysis)
import Stan.Analysis.Pretty (prettyShowAnalysis)
import Stan.Cli (InspectionArgs (..), StanArgs (..), StanCommand (..), runStanCli)
import Stan.Core.Id (Id (..))
import Stan.Hie (readHieFiles)
import Stan.Inspection (prettyShowInspection, prettyShowInspectionShort)
import Stan.Inspection.All (inspections, lookupInspectionById)
-- import Stan.Hie.Debug (debugHieFile)

import qualified Data.Map.Strict as Map


run :: IO ()
run = runStanCli >>= \case
    Stan stanArgs -> runStan stanArgs
    StanInspection inspectionArgs -> runInspection inspectionArgs

runStan :: StanArgs -> IO ()
runStan StanArgs{..} = do
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


{- | From a given path to cabal file and 'HieFile's create the map from modules
(that are in .cabal file) to the resulting parsed extensions for each.
-}
createCabalExtensionsMap
    :: Maybe FilePath
    -> [HieFile]
    -> IO (Map FilePath (Either ExtensionsError ParsedExtensions))
createCabalExtensionsMap cabalPath hies = case cabalPath of
    -- if cabal file specified via CLI option
    Just cabal ->
        ifM (doesFileExist cabal)
        {- then -} (getExtensionsWithCabal cabal)
        {- else -} (errorMessage (".cabal file does not exist: " <> toText cabal) >> exitFailure)
    -- try to find cabal file in current directory
    Nothing -> findCabalFile >>= \case
        Just cabal -> getExtensionsWithCabal cabal
        -- if cabal file is not found, pass the empty map instead
        Nothing    -> do
            warningMessage ".cabal file not found in the current directory."
            infoMessage " 💡 Try using --cabal-file-path option to specify the path to the .cabal file.\n"
            pure mempty
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

-- | Find a @.cabal@ file in the current directory.
-- TODO: better error handling in stan.
findCabalFile :: IO (Maybe FilePath)
findCabalFile = do
    dirPath <- getCurrentDirectory
    dirContent <- listDirectory dirPath
    let cabalFiles = filter (\p -> takeExtension p == ".cabal") dirContent
    pure $ case cabalFiles of
        []          -> Nothing -- throwError $ NoCabalFile dirPath
        [cabalFile] -> Just $ dirPath </> cabalFile
        x:_xs       -> Just x -- throwError $ MultipleCabalFiles (x :| xs)
