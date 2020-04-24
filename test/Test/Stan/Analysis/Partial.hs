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
        checkObservation analysis Partial.stan0001 10 12 16
    it "STAN-0002: finds usage of 'base/tail'" $
        checkObservation analysis Partial.stan0002 13 12 16
    it "STAN-0003: finds usage of 'base/init'" $
        checkObservation analysis Partial.stan0003 16 12 16
    it "STAN-0004: finds usage of 'base/last'" $
        checkObservation analysis Partial.stan0004 19 12 16
    it "STAN-0005: finds usage of 'base/!!'" $
        checkObservation analysis Partial.stan0005 22 16 18
    it "STAN-0006: finds usage of 'base/cycle'" $
        checkObservation analysis Partial.stan0006 25 13 18
    it "STAN-0007: finds usage of 'base/genericIndex'" $
        checkObservation analysis Partial.stan0007 28 20 32
    it "STAN-0008: finds usage of 'base/fromJust'" $
        checkObservation analysis Partial.stan0008 31 16 24
    it "STAN-0009: finds usage of 'base/read'" $
        checkObservation analysis Partial.stan0009 34 12 16
