module Test.Stan.Number
    ( linesOfCodeSpec
    , modulesNumSpec
    ) where

import HieTypes (HieFile (..))
import Test.Hspec (Spec, describe, it, shouldBe)

import Stan.Hie (countLinesOfCode)


linesOfCodeSpec :: HieFile -> Spec
linesOfCodeSpec hieFile = describe "LoC tests" $
    it "should count lines of code in the example file" $
        countLinesOfCode hieFile `shouldBe` 74

modulesNumSpec :: Int -> Spec
modulesNumSpec num = describe "Modules number tests" $
    it "should count correct number of modules" $
        num `shouldBe` 46
