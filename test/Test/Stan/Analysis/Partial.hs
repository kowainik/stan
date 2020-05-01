module Test.Stan.Analysis.Partial
    ( analysisPartialSpec
    ) where

import Test.Hspec (Spec, describe, it)

import Stan.Analysis (Analysis)
import Test.Stan.Analysis.Common (observationAssert)

import qualified Stan.Inspection.Partial as Partial


analysisPartialSpec :: Analysis -> Spec
analysisPartialSpec analysis = describe "Partial functions" $ do
    let checkObservation = observationAssert
            "Target/Partial.hs"
            "Target.Partial"

    it "STAN-0001: finds usage of 'base/head'" $
        checkObservation analysis Partial.stan0001 11 12 16
    it "STAN-0002: finds usage of 'base/tail'" $
        checkObservation analysis Partial.stan0002 14 12 16
    it "STAN-0003: finds usage of 'base/init'" $
        checkObservation analysis Partial.stan0003 17 12 16
    it "STAN-0004: finds usage of 'base/last'" $
        checkObservation analysis Partial.stan0004 20 12 16
    it "STAN-0005: finds usage of 'base/!!'" $
        checkObservation analysis Partial.stan0005 23 16 18
    it "STAN-0006: finds usage of 'base/cycle'" $
        checkObservation analysis Partial.stan0006 26 13 18
    it "STAN-0007: finds usage of 'base/genericIndex'" $
        checkObservation analysis Partial.stan0007 29 20 32
    it "STAN-0008: finds usage of 'base/fromJust'" $
        checkObservation analysis Partial.stan0008 32 16 24
    it "STAN-0009: finds usage of 'base/read'" $
        checkObservation analysis Partial.stan0009 35 12 16
    it "STAN-0010: finds usage of 'base/succ'" $
        checkObservation analysis Partial.stan0010 38 12 16
    it "STAN-0011: finds usage of 'base/pred'" $
        checkObservation analysis Partial.stan0011 41 12 16
    it "STAN-0012: finds usage of 'base/toEnum'" $
        checkObservation analysis Partial.stan0012 44 14 20
    it "STAN-0013: finds usage of 'base/maximum'" $
        checkObservation analysis Partial.stan0013 47 15 22
    it "STAN-0014: finds usage of 'base/minimum'" $
        checkObservation analysis Partial.stan0014 50 15 22
    it "STAN-0015: finds usage of 'base/maximumBy'" $
        checkObservation analysis Partial.stan0015 53 17 26
    it "STAN-0016: finds usage of 'base/minimumBy'" $
        checkObservation analysis Partial.stan0016 56 17 26
    it "STAN-0017: finds usage of 'base/foldl1'" $
        checkObservation analysis Partial.stan0017 59 14 20
    it "STAN-0018: finds usage of 'base/foldl1'" $
        checkObservation analysis Partial.stan0018 62 15 22
    it "STAN-0019: finds usage of 'base/foldr1'" $
        checkObservation analysis Partial.stan0019 65 14 20
