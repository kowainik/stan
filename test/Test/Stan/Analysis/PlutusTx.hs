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
    checkObservation AntiPattern.plustan01 55 12 35

  it "PLU-STAN-02: PlutusTx.UnsafeFromData unsafeFromBuiltinData" $
    checkObservation AntiPattern.plustan02 59 3 27

  it "PLU-STAN-03: No usage of Optional types in on-chain code" $
    checkObservation AntiPattern.plustan03 63 7 22

  it "PLU-STAN-04: == on pubKeyHash" $
    checkObservation AntiPattern.plustan04 67 27 29

  it "PLU-STAN-04: == on scriptHash" $
    checkObservation AntiPattern.plustan04 73 27 29

  it "PLU-STAN-04: == on credentialHash" $
    checkObservation AntiPattern.plustan04 79 35 37

  it "PLU-STAN-04: < on credentialHash" $
    checkObservation AntiPattern.plustan04 85 35 36

  it "PLU-STAN-05: higher-order list helpers" $
    checkObservation AntiPattern.plustan05 92 3 16

  it "PLU-STAN-06: nested list traversals" $
    checkObservation AntiPattern.plustan06 101 6 57

  it "PLU-STAN-07: guard syntax in on-chain code" $
    checkObservation AntiPattern.plustan07 105 5 11

  it "PLU-STAN-08: non-strict let binding used multiple times" $
    checkObservation AntiPattern.plustan08 112 7 19

  it "PLU-STAN-08: strict let binding does not trigger" $
    noObservationAssert ["PlutusTx"] analysis AntiPattern.plustan08 116

  it "PLU-STAN-08: non-strict binding used in sibling let bindings" $
    checkObservation AntiPattern.plustan08 122 7 19

  it "PLU-STAN-08: non-strict binding used in a let binding and the body" $
    checkObservation AntiPattern.plustan08 129 7 19

  it "PLU-STAN-09: valueOf compared directly" $
    checkObservation AntiPattern.plustan09 135 3 50

  it "PLU-STAN-09: valueOf compared via let bindings" $
    checkObservation AntiPattern.plustan09 148 6 39

  it "PLU-STAN-09: valueOf compared with literal on the left" $
    checkObservation AntiPattern.plustan09 161 3 50

  it "PLU-STAN-09: valueOf compared via prefix (==)" $
    checkObservation AntiPattern.plustan09 172 3 54

  it "PLU-STAN-09: valueOf compared via section (== 5000)" $
    checkObservation AntiPattern.plustan09 183 3 54

  it "PLU-STAN-10: PubKeyHash from unsafeFromBuiltinData used in comparison" $
    checkObservation AntiPattern.plustan10 194 3 103

  it "PLU-STAN-10: ScriptHash from unsafeFromBuiltinData used in comparison" $
    checkObservation AntiPattern.plustan10 201 3 103

  it "PLU-STAN-10: PubKeyHash from unsafeFromBuiltinData in lending validator" $
    checkObservation AntiPattern.plustan10 232 6 68

  it "PLU-STAN-10: PubKeyHash from unsafeFromBuiltinData via pattern binding" $
    checkObservation AntiPattern.plustan10 257 6 33

  -- Test case 1: Intermediate variable binding - triggers via transitive tracking:
  -- datum = unsafeFromBuiltinData x, then mustRepayToPkh is bound from datum
  it "PLU-STAN-10: Intermediate variable binding" $
    checkObservation AntiPattern.plustan10 281 6 33

  it "PLU-STAN-10: Record field accessor function" $
    checkObservation AntiPattern.plustan10 326 6 37

  it "PLU-STAN-10: Case expression pattern binding" $
    checkObservation AntiPattern.plustan10 304 10 37

  it "PLU-STAN-10: Where clause binding" $
    checkObservation AntiPattern.plustan10 344 35 62
