module Test.Stan.Analysis
    ( analysisSpec
    ) where

import HieTypes (HieFile)
import Test.Hspec (Spec, describe)

import Stan.Analysis (runAnalysis)
import Test.Stan.Analysis.Infinite (analysisInfiniteSpec)
import Test.Stan.Analysis.Partial (analysisPartialSpec)


analysisSpec :: [HieFile] -> Spec
analysisSpec hieFiles = describe "Static Analysis" $ do
    let analysis = runAnalysis hieFiles
    analysisPartialSpec analysis
    analysisInfiniteSpec analysis
