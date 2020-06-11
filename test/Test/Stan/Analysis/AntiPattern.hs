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
        checkObservation AntiPattern.stan0201 11 19 35
    it "STAN-0201: doesn't trigger on '[0 .. length xs - 1]'" $
        noObservation AntiPattern.stan0201 14
    it "STAN-0202: finds usage of 'foldl'" $
        checkObservation AntiPattern.stan0202 17 13 18
    it "STAN-0203: finds usage of 'Data.ByteString.Char8.pack'" $
        checkObservation AntiPattern.stan0203 20 13 21
    it "STAN-0204: finds usage of 'Data.HashMap.size'" $
        checkObservation AntiPattern.stan0204 23 19 26
    it "STAN-0204: finds usage of 'length' for 'HashMap'" $
        checkObservation AntiPattern.stan0204 26 21 27
    it "STAN-0205: finds usage of 'Data.HashSet.size'" $
        checkObservation AntiPattern.stan0205 29 19 26
    it "STAN-0205: finds usage of 'length' for 'HashSet'" $
        checkObservation AntiPattern.stan0205 32 21 27

    strictFieldsSpec analysis

    it "STAN-0207: 'length' for (,)" $
        checkObservation AntiPattern.stan0207 35 19 25
    it "STAN-0207: 'null' for Maybe" $
        checkObservation AntiPattern.stan0207 38 17 21
    it "STAN-0207: 'foldr' for Either" $
        checkObservation AntiPattern.stan0207 41 19 24

strictFieldsSpec :: Analysis -> Spec
strictFieldsSpec analysis = describe "STAN-0206: Strict data type fields" $ do
    describe "Without extensions" $ do
        let checkObservation = observationAssert
                "Target/AntiPattern/Stan0206.hs"
                "Target.AntiPattern.Stan0206"
                analysis
        let noObservation = noObservationAssert
                "Target/AntiPattern/Stan0206.hs"
                "Target.AntiPattern.Stan0206"
                analysis

        it "Doesn't trigger on strict field" $
            noObservation AntiPattern.stan0206 7
        it "Finds simple lazy field" $
            checkObservation AntiPattern.stan0206 8 7 25
        it "Finds polymorphic lazy field" $
            checkObservation AntiPattern.stan0206 9 7 23
        it "Doesn't trigger on plain newtype" $
            noObservation AntiPattern.stan0206 12
        it "Doesn't trigger on a record newtype" $
            noObservation AntiPattern.stan0206 14
        it "Doesn't trigger on strict sum type field among many fields" $
            noObservation AntiPattern.stan0206 19
        it "Finds lazy field in a sum type constructor with multiple fields" $
            checkObservation AntiPattern.stan0206 20 9 12
        it "Doesn't trigger on a single strict sum type field" $
            noObservation AntiPattern.stan0206 21
        it "Finds single lazy field in a sum type with multiple constructors" $
            checkObservation AntiPattern.stan0206 22 11 15

    describe "With the 'StrictData' extension" $ do
        let noObservation = noObservationAssert
                "Target/AntiPattern/Stan0206Extensions.hs"
                "Target.AntiPattern.Stan0206Extensions"
                analysis

        it "Doesn't trigger on a simple record field" $
            noObservation AntiPattern.stan0206 9
        it "Doesn't trigger on explicitly lazy field" $
            noObservation AntiPattern.stan0206 10
        it "Doesn't trigger on plain data type" $
            noObservation AntiPattern.stan0206 13
