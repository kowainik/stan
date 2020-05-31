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
    forM_ (zip (sortById partialInspectionsMap) [14, 17 ..]) checkObservation

    let noObservation = noObservationAssert
            "target/Target/Partial.hs"
            "Target.Partial"
            analysis

    it "STAN-0010: doesn't trigger on 'succ :: Natural -> Natural'" $
        noObservation Partial.stan0010 79
    it "STAN-0011: doesn't trigger on 'pred :: Integer -> Integer'" $
        noObservation Partial.stan0011 82

-- TODO: configure message
--    it "STAN-0011: triggers on polymorphic 'pred :: Enum a => a -> a'" $
    checkObservation (Partial.stan0011, 85)
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
