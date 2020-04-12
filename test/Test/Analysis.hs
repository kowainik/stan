module Test.Analysis
    ( analysisSpec
    ) where

import HieTypes (HieFile)
import Test.Hspec (Spec, describe)

import Stan.Analysis (runAnalysis)
import Test.Analysis.Partial (analysisHeadSpec)


analysisSpec :: [HieFile] -> Spec
analysisSpec hieFiles = describe "Static Analysis" $ do
    let analysis = runAnalysis hieFiles
    analysisHeadSpec analysis
