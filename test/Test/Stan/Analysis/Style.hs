module Test.Stan.Analysis.Style
    ( analysisStyleSpec
    ) where

import Test.Hspec (Spec, describe, it)

import Stan.Analysis (Analysis)
import Test.Stan.Analysis.Common (observationAssert)

import qualified Stan.Inspection.Style as Style


analysisStyleSpec :: Analysis -> Spec
analysisStyleSpec analysis = describe "Style" $ do
    let checkObservation = observationAssert
            "Target/Style.hs"
            "Target.Style"
            analysis

    it "STAN-0301: finds operator with the missing infix" $
        checkObservation Style.stan0301 6 1 6
