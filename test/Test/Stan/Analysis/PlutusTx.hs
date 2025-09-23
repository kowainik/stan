module Test.Stan.Analysis.PlutusTx (
  analysisPlutusTxSpec,
) where

import Test.Hspec (Spec, describe, it)

import Stan.Analysis (Analysis)
import Test.Stan.Analysis.Common (observationAssert)

import qualified Stan.Inspection.AntiPattern as AntiPattern

analysisPlutusTxSpec :: Analysis -> Spec
analysisPlutusTxSpec analysis = describe "Plutus-Tx" $ do
  let checkObservation = observationAssert ["PlutusTx"] analysis

  it "PLU-STAN-01: PlutusTx.AssocMap unsafeFromList" $
    checkObservation AntiPattern.plustan01 33 12 35

  it "PLU-STAN-02: PlutusTx.UnsafeFromData unsafeFromBuiltinData" $
    checkObservation AntiPattern.plustan02 37 3 27

  it "PLU-STAN-03: No usage of Optional types in on-chain code" $
    checkObservation AntiPattern.plustan03 41 7 22

  it "PLU-STAN-04: == on pubKeyHash" $
    checkObservation AntiPattern.plustan04 45 27 29

  it "PLU-STAN-04: == on scriptHash" $
    checkObservation AntiPattern.plustan04 51 27 29

  it "PLU-STAN-04: == on credentialHash" $
    checkObservation AntiPattern.plustan04 57 35 37

  it "PLU-STAN-04: < on credentialHash" $
    checkObservation AntiPattern.plustan04 63 35 36
