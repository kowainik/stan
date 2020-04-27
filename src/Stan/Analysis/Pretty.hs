{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Pretty printing of Stan's analysis.
-}

module Stan.Analysis.Pretty
    ( prettyShowAnalysis
    ) where

import Colourista (bold, formatWith, italic)
import Relude.Extra.Map (toPairs)

import Stan.Analysis (Analysis (..))
import Stan.Core.ModuleName (ModuleName (..))
import Stan.Core.Toggle (ToggleSolution)
import Stan.Inspection.All (inspectionsMap)
import Stan.Observation (Observation (..), Observations, prettyShowObservation)

import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Slist as S


{- | Shows analysed output of Stan work.
This functions groups 'Observation's by 'FilePath' they are found in.
-}
prettyShowAnalysis :: Analysis -> ToggleSolution -> Text
prettyShowAnalysis Analysis{..} toggleSolution = groupedObservations <> summary
  where
    groupedObservations :: Text
    groupedObservations =
        Text.intercalate "\n\n"
        $ map (showByFile toggleSolution)
        $ toPairs
        $ groupObservationsByFile analysisObservations

    summary :: Text
    summary = unlines
        [ ""
        , formatWith [bold] "           Stan's Summary:"
        , top
        , alignText "Analysed modules" <> alignNum analysisModulesNum
        , mid
        , alignText "Analysed Lines of Code" <> alignNum analysisLinesOfCode
        , mid
        , alignText "Total extensions" <> alignNum (Set.size analysisUsedExtensions)
        , mid
        , alignText "Total checked inspections" <> alignNum (HM.size inspectionsMap)
        , mid
        , alignText "Total found observations" <> alignNum (length analysisObservations)
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

showByFile :: ToggleSolution -> (FilePath, Observations) -> Text
showByFile toggleSolution (file, obs) = unlines
    [ i "  File:         " <> b (toText file)
    , i "  Module:       " <> b (maybe "" (unModuleName . observationModuleName) $ S.safeHead obs)
    , i "  Observations: " <> b (show $ length obs)
    , " ┏" <> Text.replicate 78 "━"
    ]

    <> Text.intercalate (" ┃\n ┃" <> Text.replicate 78 "~" <> "\n ┃\n")
        (toList $ prettyShowObservation toggleSolution <$> S.sortOn observationLoc obs)
  where
    i, b :: Text -> Text
    i = formatWith [italic]
    b = formatWith [bold]

-- | Groups 'Observation's by the filepath.
groupObservationsByFile
    :: Observations
    -> HashMap FilePath Observations
groupObservationsByFile = flipfoldl' hmGroup mempty
  where
    hmGroup
        :: Observation
        -> HashMap FilePath Observations
        -> HashMap FilePath Observations
    hmGroup obs =
        let newObservations :: Maybe Observations -> Observations
            newObservations Nothing     = S.one obs
            newObservations (Just obss) = S.one obs <> obss
        in HM.alter (Just . newObservations) (observationFile obs)
