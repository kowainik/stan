module Test.Stan.Analysis.Performance
    ( analysisPerformanceSpec
    ) where

import Test.Hspec (Spec, describe, it)

import Stan.Analysis (Analysis)
import Test.Stan.Analysis.Common (noObservationAssert, observationAssert)

import qualified Stan.Inspection.Performance as P


analysisPerformanceSpec :: Analysis -> Spec
analysisPerformanceSpec analysis = describe "Performance" $ do
    let checkObservation = observationAssert ["Performance"] analysis
    let noObservation = noObservationAssert  ["Performance"] analysis

    it "STAN-0401: notifies on missing specialize pragma" $
        checkObservation P.stan0401 9 1 4
    it "STAN-0401: doesn't trigger when there is already specialize pragma" $
        noObservation P.stan0401 12
