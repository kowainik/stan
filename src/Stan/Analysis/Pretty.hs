{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Pretty printing of Stan's analysis.
-}

module Stan.Analysis.Pretty
    ( prettyShowAnalysis

      -- * Numbers
    , AnalysisNumbers (..)
    , ProjectHealth (..)
    , analysisToNumbers
    , prettyHealth
    , toProjectHealth
    , isPlinthObservation
    ) where

import Colourista.Short (b, i)
import Extensions (ExtensionsError, ParsedExtensions)
import Text.Printf (printf)

import Stan.Analysis (Analysis (..))
import Stan.Core.ModuleName (ModuleName (..))
import Stan.FileInfo (FileInfo (..), extensionsToText)
import Stan.Observation (Observation (..), prettyShowObservation)
import Stan.Report.Settings (OutputSettings (..), Verbosity (..))

import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Slist as S
import Stan.Core.Id (Id(unId))


{- | Shows analysed output of Stan work.
This functions groups 'Observation's by 'FilePath' they are found in.
-}
prettyShowAnalysis :: Analysis -> OutputSettings -> Text
prettyShowAnalysis an rs@OutputSettings{..} = case outputSettingsVerbosity of
    Verbose    -> groupedObservations <> summary (analysisToNumbers an)
    NonVerbose -> unlines $ toList $ prettyShowObservation rs <$> analysisObservations an
  where
    groupedObservations :: Text
    groupedObservations =
        Text.intercalate "\n\n"
        $ filter (/= "")
        $ map (showByFile rs)
        $ Map.elems
        $ analysisFileMap an

data AnalysisNumbers = AnalysisNumbers
    { anModules     :: !Int
    , anLoc         :: !Int
    , anExts        :: !Int
    , anSafeExts    :: !Int
    , anIns         :: !Int
    , anFoundObs    :: !Int
    , anIgnoredObs  :: !Int
    , anFoundPlinth :: !Int
    , anHealth      :: !Double
    }

isPlinthObservation :: Observation -> Bool
isPlinthObservation Observation{..} =
  -- observationInspectionId includes PLU-STAN
  "PLU-STAN" `Text.isInfixOf` unId observationInspectionId

analysisToNumbers :: Analysis -> AnalysisNumbers
analysisToNumbers Analysis{..} = AnalysisNumbers
    { anModules    = analysisModulesNum
    , anLoc        = analysisLinesOfCode
    , anExts       = Set.size $ fst analysisUsedExtensions
    , anSafeExts   = Set.size $ snd analysisUsedExtensions
    , anIns        = HS.size analysisInspections
    , anFoundObs   = length analysisObservations
    , anIgnoredObs = length analysisIgnoredObservations
    , anFoundPlinth = length (S.filter isPlinthObservation analysisObservations)
    , anHealth     = calculatedHealth
    }
  where
    calculatedHealth :: Double
    calculatedHealth =
        -- all inspections ignored or no observations
        if null analysisInspections || null analysisObservations
        then 100
        else
            let totalInspections = fromIntegral $ HS.size analysisInspections
                triggeredInspections =
                    fromIntegral
                    $ Set.size
                    $ Set.fromList
                    $ map observationInspectionId
                    $ toList analysisObservations

            in 100 * (1 - triggeredInspections / totalInspections)

{- | Show project health as pretty text with 2 digits after dot.
-}
prettyHealth :: Double -> Text
prettyHealth health =
    if fromIntegral (floor health :: Int) == health  -- display without decimal part
    then toText (printf "%.0f" health :: String) <> "%"
    else toText (printf "%.2f" health :: String) <> "%"

{- | Enum to describe project health depending on the value of
'anHealth'.
-}
data ProjectHealth
    = Unhealthy
    | LowHealth
    | MediumHealth
    | Healthy

-- | Calculate 'ProjectHealth'.
toProjectHealth :: Double -> ProjectHealth
toProjectHealth health
    | health >= 100 = Healthy
    | health >= 80  = MediumHealth
    | health >= 40  = LowHealth
    | otherwise     = Unhealthy

summary :: AnalysisNumbers -> Text
summary AnalysisNumbers{..} = unlines
    [ ""
    , b "           Stan's Summary:"
    , top
    , alignText "Analysed modules" <> alignNum anModules
    , mid
    , alignText "Analysed Lines of Code" <> alignNum anLoc
    , mid
    , alignText "Total Haskell2010 extensions" <> alignNum anExts
    , mid
    , alignText "Total SafeHaskell extensions" <> alignNum anSafeExts
    , mid
    , alignText "Total checked inspections" <> alignNum anIns
    , mid
    , alignText "Total found observations" <> alignNum anFoundObs
    , mid
    , alignText "Total Plinth observations" <> alignNum anFoundPlinth
    , mid
    , alignText "Total ignored observations" <> alignNum anIgnoredObs
    , mid
    , alignText "Project health" <> alignVal (prettyHealth anHealth)
    , bot
    ]
  where
    alignNum :: Int -> Text
    alignNum = alignVal . show

    alignVal :: Text -> Text
    alignVal x = " ┃ " <> Text.justifyLeft 6 ' ' x <> " ┃"

    alignText :: Text -> Text
    alignText txt ="┃ " <> Text.justifyLeft 28 ' ' txt

    separator :: Text -> Text -> Text -> Text
    separator l c r = l <> Text.replicate 30 "━" <> c <> Text.replicate 8 "━" <> r
    top, mid, bot :: Text
    top = separator "┏" "┳" "┓"
    mid = separator "┣" "╋" "┫"
    bot = separator "┗" "┻" "┛"

showByFile :: OutputSettings -> FileInfo -> Text
showByFile outputSettings FileInfo{..} = if len == 0 then "" else unlines
    [ i "  File:         " <> b (toText fileInfoPath)
    , i "  Module:       " <> b (unModuleName fileInfoModuleName)
    , i "  LoC:          " <> b (show fileInfoLoc)
    , i "  Observations: " <> b (show len)
    , i "  Extensions from .cabal: " <> b (showExts fileInfoCabalExtensions)
    , i "  Extensions from module: " <> b (showExts fileInfoExtensions)
    , " ┏" <> Text.replicate 78 "━"
    ]

    <> Text.intercalate (" ┃\n ┃" <> Text.replicate 78 "~" <> "\n ┃\n")
        (toList $ prettyShowObservation outputSettings <$> S.sortOn observationSrcSpan fileInfoObservations)
  where
    len :: Int
    len = length fileInfoObservations

    showExts :: Either ExtensionsError ParsedExtensions -> Text
    showExts = Text.intercalate ", " . extensionsToText
