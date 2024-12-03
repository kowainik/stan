module Test.Stan.Number
    ( linesOfCodeSpec
    , modulesNumSpec
    ) where

import Stan.Hie.Compat (HieFile (..))
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Stan.Hie (countLinesOfCode)


linesOfCodeSpec :: HieFile -> Spec
linesOfCodeSpec hieFile = describe "LoC tests" $
    it "should count lines of code in the example file" $
        countLinesOfCode hieFile `shouldBe` 103

modulesNumSpec :: Int -> Spec
modulesNumSpec num = describe "Modules number tests" $
    it "should count correct number of modules" $
        num `shouldSatisfy` (> 56)
