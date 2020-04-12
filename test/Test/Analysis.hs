module Test.Analysis
    ( analysisSpecs
    ) where

import HieTypes (HieFile (..))
import Test.Hspec (Spec, describe, it, shouldBe)

import Stan.Hie (countLinesOfCode)


analysisSpecs :: HieFile -> Spec
analysisSpecs hieFile = describe "Analysis tests" $
    it "should count lines of code of example file" $
        countLinesOfCode hieFile `shouldBe` 7
