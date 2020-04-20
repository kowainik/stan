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
import Relude.Extra.Group (groupBy)
import Relude.Extra.Map (toPairs)

import Stan.Analysis (Analysis (..))
import Stan.Inspection.All (inspectionsMap)
import Stan.Observation (Observation (..), prettyShowObservation)

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as Text


{- | Shows analysed output of Stan work.
This functions groups 'Observation's by 'FilePath' they are found in.
-}
prettyShowAnalysis :: Analysis -> Text
prettyShowAnalysis Analysis{..} = groupedObservations <> summary
  where
    groupedObservations :: Text
    groupedObservations =
        Text.intercalate "\n\n"
        $ map showByFile
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
        , alignText "Total extensions" <> alignNum analysisUsedExtensions
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

showByFile :: (FilePath, NonEmpty Observation) -> Text
showByFile (file, o :| obs) = unlines
    [ i "  File:         " <> b (toText file)
    , i "  Module:       " <> b (observationModuleName o)
    , i "  Observations: " <> b (show $ 1 + length obs)
    , " ┏" <> Text.replicate 78 "━"
    ]

    <> Text.intercalate (" ┃\n ┃" <> Text.replicate 78 "~" <> "\n ┃\n")
        (map prettyShowObservation $ sortWith observationLoc $ o : obs)
  where
    i, b :: Text -> Text
    i = formatWith [italic]
    b = formatWith [bold]

groupObservationsByFile
    :: [Observation]
    -> HashMap FilePath (NonEmpty Observation)
groupObservationsByFile = groupBy observationFile
