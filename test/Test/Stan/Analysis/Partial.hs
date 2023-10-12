module Test.Stan.Analysis.Partial
    ( analysisPartialSpec
    ) where

import Test.Hspec (Arg, Expectation, Spec, SpecWith, describe, it)

import Stan.Analysis (Analysis)
import Stan.Inspection (Inspection (..), sortById)
import Stan.Inspection.Partial (partialInspectionsMap)
import Stan.NameMeta (NameMeta (..))
import Test.Stan.Analysis.Common (itShouldStr, noObservationAssert, observationAssert,
                                  unsafeNameMeta)

import qualified Data.Text as T

import qualified Stan.Inspection.Partial as Partial


analysisPartialSpec :: Analysis -> Spec
analysisPartialSpec analysis = describe "Partial functions" $ do
    forM_ (zip (sortById partialInspectionsMap) [16, 19 ..]) checkObservation

    let noObservation = noObservationAssert ["Partial"] analysis

    it "STAN-0010: doesn't trigger on 'succ :: Natural -> Natural'" $
        noObservation Partial.stan0010 81
    it "STAN-0011: doesn't trigger on 'pred :: Integer -> Integer'" $
        noObservation Partial.stan0011 84
    it "STAN-0011: triggers on polymorphic 'pred :: Enum a => a -> a'" $
        checkObservationFor Partial.stan0011 87 16 20
    it "STAN-0020: triggers on 'Data.List.NonEmpty.fromList'" $
        checkObservationFor Partial.stan0020 90 18 29

  where
    checkObservation :: (Inspection, Int) -> SpecWith (Arg Expectation)
    checkObservation (ins@Inspection{..}, line) = it (itShouldStr ins) $
        checkObservationFor ins line start end
      where
        nameMeta :: NameMeta
        nameMeta = unsafeNameMeta inspectionAnalysis

        funLen, start, end :: Int
        funLen = T.length $ nameMetaName nameMeta
        start = if nameMetaName nameMeta == "!!" then funLen + 14 else funLen + 8
        end = start + funLen

    checkObservationFor :: Inspection -> Int -> Int -> Int -> Expectation
    checkObservationFor = observationAssert
        ["Partial"]
        analysis
