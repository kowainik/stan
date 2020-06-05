module Test.Stan.Analysis.AntiPattern
    ( analysisAntiPatternSpec
    ) where

import Test.Hspec (Spec, describe, it)

import Stan.Analysis (Analysis)
import Test.Stan.Analysis.Common (noObservationAssert, observationAssert)

import qualified Stan.Inspection.AntiPattern as AntiPattern


analysisAntiPatternSpec :: Analysis -> Spec
analysisAntiPatternSpec analysis = describe "Anti-patterns" $ do
    let checkObservation = observationAssert
            "Target/AntiPattern.hs"
            "Target.AntiPattern"
            analysis
    let noObservation = noObservationAssert
            "Target/AntiPattern.hs"
            "Target.AntiPattern"
            analysis

    it "STAN-0201: finds usage of '[0 .. length xs]'" $
        checkObservation AntiPattern.stan0201 9 19 35
    it "STAN-0201: doesn't trigger on '[0 .. length xs - 1]'" $
        noObservation AntiPattern.stan0201 12
    it "STAN-0202: finds usage of 'foldl'" $
        checkObservation AntiPattern.stan0202 15 13 18
    it "STAN-0203: finds usage of 'Data.ByteString.Char8.pack'" $
        checkObservation AntiPattern.stan0203 18 13 21
