module Test.Stan.Analysis.Partial
    ( analysisPartialSpec
    ) where

import Test.Hspec (Arg, Expectation, Spec, SpecWith, describe, it)

import Stan.Analysis (Analysis)
import Stan.Inspection (Inspection (..), sortById)
import Stan.Inspection.Partial (partialInspectionsMap)
import Stan.NameMeta (NameMeta (..))
import Test.Stan.Analysis.Common (itShouldStr, observationAssert, unsafeNameMeta)

import qualified Data.Text as T


analysisPartialSpec :: Analysis -> Spec
analysisPartialSpec analysis = describe "Partial functions" $
    forM_ (zip (sortById partialInspectionsMap) [14, 17 ..]) checkObservation
  where
    checkObservation :: (Inspection, Int) -> SpecWith (Arg Expectation)
    checkObservation (ins@Inspection{..}, line) = it (itShouldStr ins) $
        observationAssert "Target/Partial.hs" "Target.Partial"
            analysis
            ins
            line start end
      where
        nameMeta :: NameMeta
        nameMeta = unsafeNameMeta inspectionAnalysis

        funLen, start, end :: Int
        funLen = T.length $ nameMetaName nameMeta
        start = if nameMetaName nameMeta == "!!" then funLen + 14 else funLen + 8
        end = start + funLen
