{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Main running module.
-}

module Stan
    ( runStan
    ) where

import Stan.Analysis (runAnalysis)
import Stan.Analysis.Pretty (prettyShowAnalysis)
import Stan.Cli (InspectionArgs (..), StanArgs (..), StanCommand (..), runStanCli)
import Stan.Core.Id (Id (..))
import Stan.Hie (readHieFiles)
import Stan.Inspection (prettyShowInspection, prettyShowInspectionShort)
import Stan.Inspection.All (inspections, lookupInspectionById)
-- import Stan.Hie.Debug (debugHieFile)

import Colourista (errorMessage, formatWith, italic)


runStan :: IO ()
runStan = runStanCli >>= \case
    Stan StanArgs{..} -> do
        hieFiles <- readHieFiles stanArgsHiedir
        let analysis = runAnalysis hieFiles
        putTextLn $ prettyShowAnalysis analysis
--        debugHieFile "target/Target/Infinite.hs" hieFiles
    StanInspection InspectionArgs{..} -> case inspectionArgsId of
        Nothing  -> for_ inspections (putTextLn . prettyShowInspectionShort)
        Just insId -> case lookupInspectionById insId of
            Just ins -> putTextLn $ prettyShowInspection ins
            Nothing  -> do
                errorMessage $ "Inspection with such ID does not exist: " <> unId insId
                putTextLn $ formatWith [italic] "  Use 'stan inspection' to see the list of all available inspections."
