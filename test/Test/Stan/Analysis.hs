module Test.Stan.Analysis
    ( analysisSpec
    ) where

import HieTypes (HieFile (..))
import Test.Hspec (Spec, describe, it, runIO, shouldBe)

import Stan (createCabalExtensionsMap)
import Stan.Analysis (Analysis (..), runAnalysis)
import Test.Stan.Analysis.Infinite (analysisInfiniteSpec)
import Test.Stan.Analysis.Partial (analysisPartialSpec)

import qualified Data.Set as Set


analysisSpec :: [HieFile] -> Spec
analysisSpec hieFiles = describe "Static Analysis" $ do
    extensionsMap <- runIO $ createCabalExtensionsMap ["stan.cabal"] hieFiles
    let analysis = runAnalysis extensionsMap hieFiles
    analysisPartialSpec analysis
    analysisInfiniteSpec analysis
    analysisExtensionsSpec analysis


analysisExtensionsSpec :: Analysis -> Spec
analysisExtensionsSpec Analysis{..} = describe "Used extensions" $ do
    it "should correctly count total amount of used extensions" $
        Set.size (fst analysisUsedExtensions) `shouldBe` 14
    it "should correctly count total amount of used safe extensions" $
        Set.size (snd analysisUsedExtensions) `shouldBe` 0
