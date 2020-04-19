module Test.Stan.Inspection
    ( inspectionsSpec
    ) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Stan.Inspection.All (getInspectionById)

import qualified Stan.Inspection.Infinite as Stan
import qualified Stan.Inspection.Partial as Stan


inspectionsSpec :: Spec
inspectionsSpec = describe "Inspections by ID" $ do
    describe "Partial" $
        it "STAN-0001 should be partial head" $
            getInspectionById Stan.stan0001 `shouldBe` Stan.stan0001Inspection
    describe "Infinite" $
        it "STAN-0101 should be infinite reverse" $
            getInspectionById Stan.stan0101 `shouldBe` Stan.stan0101Inspection
