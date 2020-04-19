module Test.Stan.Inspection
    ( inspectionsSpec
    ) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Stan.Inspection.All (getInspectionById)
import Stan.Inspection.Partial (stan0001, stan0001Inspection)


inspectionsSpec :: Spec
inspectionsSpec = describe "Inspections by ID" $
    it "STAN-0001 should be partial head" $
        getInspectionById stan0001 `shouldBe` stan0001Inspection
