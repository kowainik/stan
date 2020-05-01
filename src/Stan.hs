{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Main running module.
-}

module Stan
    ( run
    ) where

import Relude.Extra.Tuple (mapToSnd)

import Colourista (errorMessage, formatWith, infoMessage, italic, warningMessage)
import Control.Exception (catch)
import Extensions (ExtensionsError (CabalParseError), ExtensionsResult)
import Extensions.Cabal (CabalException, parseCabalFileExtensions)
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
import qualified Data.Set as Set


run :: IO ()
run = runStanCli >>= \case
    Stan stanArgs -> runStan stanArgs
    StanInspection inspectionArgs -> runInspection inspectionArgs

runStan :: StanArgs -> IO ()
runStan StanArgs{..} = do
    hieFiles <- readHieFiles stanArgsHiedir
    -- create cabal default extensions map
    cabalExtensionsMap <- case stanArgsCabalFilePath of
        -- if cabal file specified via CLI option
        Just cabal ->
            ifM (doesFileExist cabal)
            {- then -} (getExtensionsWithCabal cabal hieFiles)
            {- else -} (errorMessage (".cabal file does not exist: " <> toText cabal) >> exitFailure)
        -- try to find cabal file in current directory
        Nothing -> findCabalFile >>= \case
            Just cabal -> getExtensionsWithCabal cabal hieFiles
            -- if cabal file is not found, pass the empty map instead
            Nothing    -> do
                warningMessage ".cabal file not found in the current directory."
                infoMessage " 💡 Try using --cabal-file-path option to specify the path to the .cabal file.\n"
                pure mempty

    let analysis = runAnalysis cabalExtensionsMap hieFiles
    putTextLn $ prettyShowAnalysis analysis stanArgsToggleSolution
--    debugHieFile "target/Target/Infinite.hs" hieFiles
  where
    getExtensionsWithCabal :: FilePath -> [HieFile] -> IO (Map FilePath ExtensionsResult)
    getExtensionsWithCabal cabal hies = do
        infoMessage $ "Using the following .cabal file: " <> toText cabal <> "\n"
        (Right . Set.fromList <<$>> parseCabalFileExtensions cabal)
            `catch` handleCabalErr
      where
        handleCabalErr :: CabalException -> IO (Map FilePath ExtensionsResult)
        handleCabalErr _ = do
            errorMessage "Error when parsing cabal file. Stan will continue without information from .cabal file"
            pure $ Map.fromList $
                map (mapToSnd (const $ Left CabalParseError) . hie_hs_file) hies

runInspection :: InspectionArgs -> IO ()
runInspection InspectionArgs{..} = case inspectionArgsId of
    Nothing  -> for_ inspections (putTextLn . prettyShowInspectionShort)
    Just insId -> case lookupInspectionById insId of
        Just ins -> putTextLn $ prettyShowInspection ins
        Nothing  -> do
            errorMessage $ "Inspection with such ID does not exist: " <> unId insId
            putTextLn $ " 💡 " <> formatWith [italic] "Use 'stan inspection' to see the list of all available inspections."

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
