module Test.Stan.Analysis.Style
    ( analysisStyleSpec
    ) where

import Test.Hspec (Spec, describe, it)

import Stan.Analysis (Analysis)
import Test.Stan.Analysis.Common (noObservationAssert, observationAssert)

import qualified Stan.Inspection.Style as Style


analysisStyleSpec :: Analysis -> Spec
analysisStyleSpec analysis = describe "Style" $ do
    let checkObservation = observationAssert ["Style"] analysis
    let noObservation = noObservationAssert ["Style"] analysis

    -- fixity
    it "STAN-0301: finds operator with the missing infix" $
        checkObservation Style.stan0301 6 1 6
    it "STAN-0301: no warning when fixity is declared" $
        noObservation Style.stan0301 10

    -- big tuples
    it "STAN-0302: triggers on tuple with 4 elements in the type signature" $
        checkObservation Style.stan0302 13 16 36
    it "STAN-0302: triggers on tuple with 4 elements in the literal" $
        checkObservation Style.stan0302 14 10 22
    it "STAN-0302: no observation on triple in type" $
        noObservation Style.stan0302 16
    it "STAN-0302: no observation on triple literal" $
        noObservation Style.stan0302 17
    it "STAN-0302: triggers on tuple with 4 elements as a record field" $
        checkObservation Style.stan0302 20 20 40
