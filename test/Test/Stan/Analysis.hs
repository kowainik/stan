module Test.Stan.Analysis
    ( analysisSpec
    ) where

import Extensions (getPackageExtentionsBySources)
import HieTypes (HieFile (..))
import Test.Hspec (Spec, describe, it, runIO, shouldBe)

import Stan.Analysis (Analysis (..), runAnalysis)
import Test.Stan.Analysis.Infinite (analysisInfiniteSpec)
import Test.Stan.Analysis.Partial (analysisPartialSpec)

import qualified Data.Set as Set


analysisSpec :: [HieFile] -> Spec
analysisSpec hieFiles = describe "Static Analysis" $ do
    let sourcesMap = fromList $ map (\HieFile{..} -> (hie_hs_file, hie_hs_src)) hieFiles
    extensionsMap <- runIO $ getPackageExtentionsBySources "stan.cabal" sourcesMap
    let analysis = runAnalysis extensionsMap hieFiles
    analysisPartialSpec analysis
    analysisInfiniteSpec analysis
    analysisExtensionsSpec analysis


analysisExtensionsSpec :: Analysis -> Spec
analysisExtensionsSpec Analysis{..} = describe "Used extensions" $
    it "should correctly count total amount of used extensions" $
        Set.size analysisUsedExtensions `shouldBe` 14
