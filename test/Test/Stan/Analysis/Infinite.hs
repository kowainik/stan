module Test.Stan.Analysis.Infinite
    ( analysisInfiniteSpec
    ) where

import Test.Hspec (Arg, Expectation, Spec, SpecWith, describe, it)

import Stan.Analysis (Analysis)
import Stan.Inspection (Inspection (..), sortById)
import Stan.Inspection.Infinite (infiniteInspectionsMap)
import Stan.NameMeta (NameMeta (..))
import Test.Stan.Analysis.Common (itShouldStr, observationAssert, unsafeNameMeta)

import qualified Data.Text as T


analysisInfiniteSpec :: Analysis -> Spec
analysisInfiniteSpec analysis = describe "Infinite functions" $
    forM_ (zip (sortById infiniteInspectionsMap) [9, 12 ..]) checkObservation
  where
    checkObservation :: (Inspection, Int) -> SpecWith (Arg Expectation)
    checkObservation (ins@Inspection{..}, line) = it (itShouldStr ins) $
        observationAssert "Target/Infinite.hs" "Target.Infinite"
            analysis
            ins
            line start end
      where
        nameMeta :: NameMeta
        nameMeta = unsafeNameMeta inspectionAnalysis

        funLen, start, end :: Int
        funLen = T.length $ nameMetaName nameMeta
        start  = funLen + 8
        end    = start + funLen
