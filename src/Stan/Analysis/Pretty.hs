{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Pretty printing of Stan's analysis.
-}

module Stan.Analysis.Pretty
    ( prettyShowAnalysis
    ) where

import Colourista.Short (b, i)

import Stan.Analysis (Analysis (..))
import Stan.Core.ModuleName (ModuleName (..))
import Stan.FileInfo (FileInfo (..))
import Stan.Observation (Observation (..), prettyShowObservation)
import Stan.Report.Settings (ReportSettings)

import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Slist as S


{- | Shows analysed output of Stan work.
This functions groups 'Observation's by 'FilePath' they are found in.
-}
prettyShowAnalysis :: Analysis -> ReportSettings -> Text
prettyShowAnalysis an reportSettings = groupedObservations <> summary (analysisToNumbers an)
  where
    groupedObservations :: Text
    groupedObservations =
        Text.intercalate "\n\n"
        $ filter (/= "")
        $ map (showByFile reportSettings)
        $ Map.elems
        $ analysisFileMap an

data AnalysisNumbers = AnalysisNumbers
    { anModules    :: !Int
    , anLoc        :: !Int
    , anExts       :: !Int
    , anSafeExts   :: !Int
    , anIns        :: !Int
    , anFoundObs   :: !Int
    , anIgnoredObs :: !Int
    }

analysisToNumbers :: Analysis -> AnalysisNumbers
analysisToNumbers Analysis{..} = AnalysisNumbers
    { anModules    = analysisModulesNum
    , anLoc        = analysisLinesOfCode
    , anExts       = Set.size $ fst analysisUsedExtensions
    , anSafeExts   = Set.size $ snd analysisUsedExtensions
    , anIns        = HS.size analysisInspections
    , anFoundObs   = length analysisObservations
    , anIgnoredObs = length analysisIgnoredObservations
    }


summary :: AnalysisNumbers -> Text
summary AnalysisNumbers{..} = unlines
    [ ""
    , b "           Stan's Summary:"
    , top
    , alignText "Analysed modules" <> alignNum anModules
    , mid
    , alignText "Analysed Lines of Code" <> alignNum anLoc
    , mid
    , alignText "Total Haskel2010 extensions" <> alignNum anExts
    , mid
    , alignText "Total SafeHaskel extensions" <> alignNum anSafeExts
    , mid
    , alignText "Total checked inspections" <> alignNum anIns
    , mid
    , alignText "Total found observations" <> alignNum anFoundObs
    , mid
    , alignText "Total ignored observations" <> alignNum anIgnoredObs
    , bot
    ]
  where
    alignNum :: Int -> Text
    alignNum x = " ┃ " <> Text.justifyLeft 6 ' ' (show x) <> " ┃"

    alignText :: Text -> Text
    alignText txt ="┃ " <> Text.justifyLeft 27 ' ' txt

    separator :: Text -> Text -> Text -> Text
    separator l c r = l <> Text.replicate 29 "━" <> c <> Text.replicate 8 "━" <> r
    top, mid, bot :: Text
    top = separator "┏" "┳" "┓"
    mid = separator "┣" "╋" "┫"
    bot = separator "┗" "┻" "┛"

showByFile :: ReportSettings -> FileInfo -> Text
showByFile reportSettings FileInfo{..} = if len == 0 then "" else unlines
    [ i "  File:         " <> b (toText fileInfoPath)
    , i "  Module:       " <> b (maybe "" (unModuleName . observationModuleName) $ S.safeHead fileInfoObservations)
    , i "  LoC:          " <> b (show fileInfoLoc)
    , i "  Observations: " <> b (show len)
    , " ┏" <> Text.replicate 78 "━"
    ]

    <> Text.intercalate (" ┃\n ┃" <> Text.replicate 78 "~" <> "\n ┃\n")
        (toList $ prettyShowObservation reportSettings <$> S.sortOn observationLoc fileInfoObservations)
  where
    len :: Int
    len = length fileInfoObservations
