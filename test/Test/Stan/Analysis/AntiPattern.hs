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
        checkObservation AntiPattern.stan0201 16 19 35
    it "STAN-0201: doesn't trigger on '[0 .. length xs - 1]'" $
        noObservation AntiPattern.stan0201 19
    it "STAN-0202: finds usage of 'foldl'" $
        checkObservation AntiPattern.stan0202 22 13 18
    it "STAN-0203: finds usage of 'Data.ByteString.Char8.pack'" $
        checkObservation AntiPattern.stan0203 25 13 21
    it "STAN-0204: finds usage of 'Data.HashMap.size'" $
        checkObservation AntiPattern.stan0204 28 19 26
    it "STAN-0204: finds usage of 'length' for 'HashMap'" $
        checkObservation AntiPattern.stan0204 31 21 27
    it "STAN-0205: finds usage of 'Data.HashSet.size'" $
        checkObservation AntiPattern.stan0205 34 19 26
    it "STAN-0205: finds usage of 'length' for 'HashSet'" $
        checkObservation AntiPattern.stan0205 37 21 27

    strictFieldsSpec analysis

    it "STAN-0207: 'length' for (,)" $
        checkObservation AntiPattern.stan0207 40 19 25
    it "STAN-0207: 'null' for Maybe" $
        checkObservation AntiPattern.stan0207 43 17 21
    it "STAN-0207: 'foldr' for Either" $
        checkObservation AntiPattern.stan0207 46 19 24

    it "STAN-0208: finds usage of 'length' for 'Text'" $
        checkObservation AntiPattern.stan0208 49 18 29
    it "STAN-0209: finds usage of 'nub' for lists" $
        checkObservation AntiPattern.stan0209 52 11 19
    it "STAN-0210: finds usage of 'for_' for ranges" $
        checkObservation AntiPattern.stan0210 55 12 35
    it "STAN-0210: finds usage of 'forM_' for ranges" $
        checkObservation AntiPattern.stan0210 58 15 29

    it "STAN-0211: finds usage of 'http://' '</>'" $
        checkObservation AntiPattern.stan0211 61 12 41
    it "STAN-0211: finds usage of 'fooUrl' '</>'" $
        checkObservation AntiPattern.stan0211 64 12 28
    it "STAN-0211: finds usage of '</>' 'fooUrl'" $
        checkObservation AntiPattern.stan0211 70 12 28
    it "STAN-0211: doesn't trigger on 'fooUral' '</>'" $
        noObservation AntiPattern.stan0211 76

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
