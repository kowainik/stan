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

import qualified Stan.Inspection.Infinite as Stan

analysisInfiniteSpec :: Analysis -> Spec
analysisInfiniteSpec analysis = describe "Infinite functions" $ do
    forM_ (zip (sortById infiniteInspectionsMap) [11, 14 ..]) checkObservationAuto
    checkObservation Stan.stan0103 29 23 38
  where
    checkObservation :: Inspection -> Int -> Int -> Int -> SpecWith (Arg Expectation)
    checkObservation ins@Inspection{..} l st = it (itShouldStr ins) .
        observationAssert "Target/Infinite.hs" "Target.Infinite" analysis ins l st

    checkObservationAuto :: (Inspection, Int) -> SpecWith (Arg Expectation)
    checkObservationAuto (ins@Inspection{..}, line) = checkObservation ins line start end
      where
        nameMeta :: NameMeta
        nameMeta = unsafeNameMeta inspectionAnalysis

        funLen, start, end :: Int
        funLen = T.length $ nameMetaName nameMeta
        start  = funLen + 8
        end    = start + funLen
