module Test.Stan.Inspection
    ( inspectionsSpec
    ) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Stan.Inspection.All (getInspectionById)

import qualified Stan.Inspection.Infinite as Stan
import qualified Stan.Inspection.Partial as Stan


inspectionsSpec :: Spec
inspectionsSpec = describe "Inspections by ID" $ do
    describe "Partial" $ do
        it "STAN-0001 should be partial head" $
            getInspectionById Stan.stan0001 `shouldBe` Stan.stan0001Inspection
        it "STAN-0002 should be partial tail" $
            getInspectionById Stan.stan0002 `shouldBe` Stan.stan0002Inspection
        it "STAN-0003 should be partial init" $
            getInspectionById Stan.stan0003 `shouldBe` Stan.stan0003Inspection
        it "STAN-0004 should be partial last" $
            getInspectionById Stan.stan0004 `shouldBe` Stan.stan0004Inspection

    describe "Infinite" $
        it "STAN-0101 should be infinite reverse" $
            getInspectionById Stan.stan0101 `shouldBe` Stan.stan0101Inspection
