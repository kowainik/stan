module Test.Stan.Analysis.Infinite
    ( analysisInfiniteSpec
    ) where

import Test.Hspec (Spec, describe, it)

import Stan.Analysis (Analysis)
import Test.Stan.Analysis.Common (observationAssert)

import qualified Stan.Inspection.Infinite as Infinite


analysisInfiniteSpec :: Analysis -> Spec
analysisInfiniteSpec analysis = describe "Partial functions" $ do
    let checkObservation = observationAssert
            "Target/Infinite.hs"
            "Target.Infinite"

    it "STAN-0101: finds usage of 'base/reverse'" $
        checkObservation analysis Infinite.stan0101 9 15 22
    it "STAN-0102: finds usage of 'base/isSuffixOf'" $
        checkObservation analysis Infinite.stan0102 12 18 28
    it "STAN-0103: finds usage of 'base/genericLength'" $
        checkObservation analysis Infinite.stan0103 15 21 34
