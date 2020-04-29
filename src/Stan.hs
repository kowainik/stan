{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Main running module.
-}

module Stan
    ( run
    ) where

import Colourista (errorMessage, formatWith, infoMessage, italic, warningMessage)
import Extensions (ExtensionsResult, getPackageExtentionsBySources)
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


run :: IO ()
run = runStanCli >>= \case
    Stan stanArgs -> runStan stanArgs
    StanInspection inspectionArgs -> runInspection inspectionArgs

runStan :: StanArgs -> IO ()
runStan StanArgs{..} = do
    hieFiles <- readHieFiles stanArgsHiedir
    -- create extensions map
    extensionsMap <- case stanArgsCabalFilePath of
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

    let analysis = runAnalysis extensionsMap hieFiles
    putTextLn $ prettyShowAnalysis analysis stanArgsToggleSolution
--    debugHieFile "target/Target/Infinite.hs" hieFiles
  where
    getExtensionsWithCabal :: FilePath -> [HieFile] -> IO (Map FilePath ExtensionsResult)
    getExtensionsWithCabal cabal hies = do
        -- create Map from filepathes to sources.
        let sourcesMap = fromList $ map (\HieFile{..} -> (hie_hs_file, hie_hs_src)) hies

        infoMessage $ "Using the following .cabal file: " <> toText cabal <> "\n"
        getPackageExtentionsBySources cabal sourcesMap

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
