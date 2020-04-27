{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Main running module.
-}

module Stan
    ( run
    ) where

import Colourista (errorMessage, formatWith, italic)
import Extensions (getPackageExtentionsBySources)
import HieTypes (HieFile (..))

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
    -- create Map from filepathes to sources.
    let sourcesMap = fromList $ map (\HieFile{..} -> (hie_hs_file, hie_hs_src)) hieFiles
    -- create extensions map
    extensionsMap <- getPackageExtentionsBySources "stan.cabal" sourcesMap

    let analysis = runAnalysis extensionsMap hieFiles
    putTextLn $ prettyShowAnalysis analysis stanArgsToggleSolution
--    debugHieFile "target/Target/Infinite.hs" hieFiles

runInspection :: InspectionArgs -> IO ()
runInspection InspectionArgs{..} = case inspectionArgsId of
    Nothing  -> for_ inspections (putTextLn . prettyShowInspectionShort)
    Just insId -> case lookupInspectionById insId of
        Just ins -> putTextLn $ prettyShowInspection ins
        Nothing  -> do
            errorMessage $ "Inspection with such ID does not exist: " <> unId insId
            putTextLn $ formatWith [italic] "  Use 'stan inspection' to see the list of all available inspections."
