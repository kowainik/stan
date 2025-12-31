module Test.Stan.Analysis.PlutusTx (
  analysisPlutusTxSpec,
) where

import Test.Hspec (Spec, describe, it)

import Stan.Analysis (Analysis)
import Test.Stan.Analysis.Common (noObservationAssert, observationAssert)

import qualified Stan.Inspection.AntiPattern as AntiPattern

analysisPlutusTxSpec :: Analysis -> Spec
analysisPlutusTxSpec analysis = describe "Plutus-Tx" $ do
  let checkObservation = observationAssert ["PlutusTx"] analysis

  it "PLU-STAN-01: PlutusTx.AssocMap unsafeFromList" $
    checkObservation AntiPattern.plustan01 38 12 35

  it "PLU-STAN-02: PlutusTx.UnsafeFromData unsafeFromBuiltinData" $
    checkObservation AntiPattern.plustan02 42 3 27

  it "PLU-STAN-03: No usage of Optional types in on-chain code" $
    checkObservation AntiPattern.plustan03 46 7 22

  it "PLU-STAN-04: == on pubKeyHash" $
    checkObservation AntiPattern.plustan04 50 27 29

  it "PLU-STAN-04: == on scriptHash" $
    checkObservation AntiPattern.plustan04 56 27 29

  it "PLU-STAN-04: == on credentialHash" $
    checkObservation AntiPattern.plustan04 62 35 37

  it "PLU-STAN-04: < on credentialHash" $
    checkObservation AntiPattern.plustan04 68 35 36

  it "PLU-STAN-05: higher-order list helpers" $
    checkObservation AntiPattern.plustan05 75 3 16

  it "PLU-STAN-06: nested list traversals" $
    checkObservation AntiPattern.plustan06 84 6 57

  it "PLU-STAN-07: guard syntax in on-chain code" $
    checkObservation AntiPattern.plustan07 88 5 11

  it "PLU-STAN-08: non-strict let binding used multiple times" $
    checkObservation AntiPattern.plustan08 95 7 19

  it "PLU-STAN-08: strict let binding does not trigger" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan08 100

  it "PLU-STAN-08: non-strict binding used in sibling let bindings" $
    checkObservation AntiPattern.plustan08 105 7 19

  it "PLU-STAN-08: non-strict binding used in a let binding and the body" $
    checkObservation AntiPattern.plustan08 112 7 19

  it "PLU-STAN-09: valueOf compared directly" $
    checkObservation AntiPattern.plustan09 118 3 50

  it "PLU-STAN-09: valueOf compared via let bindings" $
    checkObservation AntiPattern.plustan09 131 6 39

  it "PLU-STAN-09: valueOf compared with literal on the left" $
    checkObservation AntiPattern.plustan09 144 3 50

  it "PLU-STAN-09: valueOf compared via prefix (==)" $
    checkObservation AntiPattern.plustan09 155 3 54

  it "PLU-STAN-09: valueOf compared via section (== 5000)" $
    checkObservation AntiPattern.plustan09 166 3 54
