module Test.Stan.Analysis.AntiPattern
    ( analysisAntiPatternSpec
    ) where

import Test.Hspec (Spec, describe, it, xit)

import Stan.Analysis (Analysis)
import Test.Stan.Analysis.Common (noObservationAssert, observationAssert, observationAssertMulti)

import qualified Stan.Inspection.AntiPattern as AntiPattern


analysisAntiPatternSpec :: Analysis -> Spec
analysisAntiPatternSpec analysis = describe "Anti-patterns" $ do
    let checkObservation = observationAssert ["AntiPattern"] analysis
    let noObservation = noObservationAssert ["AntiPattern"] analysis

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

    unsafeFunctionsSpec analysis
    patternMatchSpec analysis
    compareSpec analysis

    it "STAN-0215: finds usage of '/' in '</>' path left" $
        checkObservation AntiPattern.stan0215 82 19 38
    it "STAN-0215: finds usage of '/' in '</>' path right" $
        checkObservation AntiPattern.stan0215 88 20 39
    it "STAN-0215: finds usage of '\\' in '</>' path left" $
        checkObservation AntiPattern.stan0215 85 22 42
    it "STAN-0215: finds usage of '\\' in '</>' path right" $
        checkObservation AntiPattern.stan0215 91 23 43
    it "STAN-0215: don't triggered when no slashes" $
        noObservation AntiPattern.stan0215 94

    it "STAN-0216: finds usage of String in type signatures" $
        checkObservation AntiPattern.stan0216 96 22 28
    it "STAN-0216: finds usage of String in function return signatures" $
        checkObservation AntiPattern.stan0216 99 37 43
    it "STAN-0216: finds usage of String in data type fields" $
        checkObservation AntiPattern.stan0216 102 34 40

strictFieldsSpec :: Analysis -> Spec
strictFieldsSpec analysis = describe "STAN-0206: Strict data type fields" $ do
    describe "Without extensions" $ do
        let checkObservation = observationAssert ["AntiPattern", "Stan0206"] analysis
        let noObservation = noObservationAssert ["AntiPattern", "Stan0206"] analysis

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
        it "Doesn't trigger on forall wo constraint with 1 var" $
            noObservation AntiPattern.stan0206 25
        it "Finds single lazy field after forall wo constraint with 1 var" $
            checkObservation AntiPattern.stan0206 26 16 17
        it "Doesn't trigger on forall wo constraint with 2 var" $
            noObservation AntiPattern.stan0206 27
        it "Finds single lazy field after forall wo constraint with 2 var" $
            checkObservation AntiPattern.stan0206 28 17 18
        it "Doesn't trigger on strict field after forall wo constraint with 2 var" $
            noObservation AntiPattern.stan0206 29
        it "Doesn't trigger on forall constraint with 1 var" $
            noObservation AntiPattern.stan0206 30
        it "Finds single lazy field after forall constraint with 1 var" $
            checkObservation AntiPattern.stan0206 31 17 18
        it "Doesn't trigger on forall constraint with 2 var" $
            noObservation AntiPattern.stan0206 32
        it "Finds single lazy field after forall constraint with 2 var" $
            checkObservation AntiPattern.stan0206 33 17 18
        it "Doesn't trigger on strict field after forall constraint with 2 var" $
            noObservation AntiPattern.stan0206 34

        it "Doesn't trigger on forall constraint on record type" $
            noObservation AntiPattern.stan0206 36
        it "Findsa lazy Int field after forall constraint on record type" $
            checkObservation AntiPattern.stan0206 38 7 19
        it "Finds a lazy constrainted field after forall on record type" $
            checkObservation AntiPattern.stan0206 39 7 17

    describe "With the 'StrictData' extension" $ do
        let noObservation = noObservationAssert ["AntiPattern", "Stan0206Extensions"] analysis

        it "Doesn't trigger on a simple record field" $
            noObservation AntiPattern.stan0206 9
        it "Doesn't trigger on explicitly lazy field" $
            noObservation AntiPattern.stan0206 10
        it "Doesn't trigger on plain data type" $
            noObservation AntiPattern.stan0206 13

unsafeFunctionsSpec :: Analysis -> Spec
unsafeFunctionsSpec analysis = describe "STAN-0212: Unsafe functions" $ do
    let checkObservation = observationAssert ["AntiPattern", "Stan0212"] analysis

    it "Find: undefined" $
        checkObservation AntiPattern.stan0212 10 17 26
    it "Find: unsafeCoerce" $
        checkObservation AntiPattern.stan0212 13 20 32
    it "Find: unsafePerformIO" $
        checkObservation AntiPattern.stan0212 16 23 38
    it "Find: unsafeInterleaveIO" $
        checkObservation AntiPattern.stan0212 19 26 44
    it "Find: unsafeDupablePerformIO" $
        checkObservation AntiPattern.stan0212 22 30 52
    it "Find: unsafeFixIO" $
        checkObservation AntiPattern.stan0212 25 19 30

patternMatchSpec :: Analysis -> Spec
patternMatchSpec analysis = describe "STAN-0212: Pattern Matching on _" $ do
    let checkObservation = observationAssert ["AntiPattern", "Stan0213"] analysis
    let noObservation = noObservationAssert ["AntiPattern", "Stan0213"] analysis

    it "for lambda case" $
        checkObservation AntiPattern.stan0213 12 5 17
    it "not triggered for lambda case on integers" $
        noObservation AntiPattern.stan0213 17
    it "for case" $
        checkObservation AntiPattern.stan0213 23 5 18
    it "not triggered for case on strings" $
        noObservation AntiPattern.stan0213 29
    it "not triggered for case on all constructors" $ do
        noObservation AntiPattern.stan0213 33
        noObservation AntiPattern.stan0213 34
        noObservation AntiPattern.stan0213 35

    it "not triggered for lambda case on all constructors" $ do
        noObservation AntiPattern.stan0213 39
        noObservation AntiPattern.stan0213 40
        noObservation AntiPattern.stan0213 41

    it "for case on maybe" $
        checkObservation AntiPattern.stan0213 46 5 20
    it "for lambda case on maybe" $
        checkObservation AntiPattern.stan0213 51 5 20
    it "not triggered for lambda case on maybe with full pm" $
        noObservation AntiPattern.stan0213 56
    it "not triggered for lambda case on one branch _" $
        noObservation AntiPattern.stan0213 60

    it "not triggered for lambda case on Chars" $
        noObservation AntiPattern.stan0213 65

compareSpec :: Analysis -> Spec
compareSpec analysis = describe "STAN-0214: Replace multiple comparison operators" $ do
    let checkObservation = observationAssertMulti
            ["AntiPattern", "Stan0214"]
            analysis
            AntiPattern.stan0214
    let noObservation = noObservationAssert
            ["AntiPattern", "Stan0214"]
            analysis
            AntiPattern.stan0214

    it "Finds: < and >" $
        checkObservation 7 1 10 23
    it "Finds: == and <" $
        checkObservation 13 1 16 23
    it "No warning on: == on different expressions" $ do
        noObservation 19
        noObservation 20
        noObservation 21
        noObservation 22
    it "No warning on: Single >=" $ do
        noObservation 25
        noObservation 26
        noObservation 27
    it "No warning on: >= with different constants" $ do
        noObservation 30
        noObservation 31
        noObservation 32
        noObservation 33
        noObservation 34
        noObservation 35
    xit "Handles functions with pattern-matching" $
        checkObservation 41 1 44 20
