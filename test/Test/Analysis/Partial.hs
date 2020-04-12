module Test.Analysis.Partial
    ( analysisHeadSpec
    ) where

import SrcLoc (mkRealSrcLoc, mkRealSrcSpan)
import Test.Hspec (Spec, describe, it, shouldBe)

import Stan.Analysis (Analysis (..))
import Stan.Core.Id (Id (..))
import Stan.Observation (Observation (..))


analysisHeadSpec :: Analysis -> Spec
analysisHeadSpec analysis = describe "STAN-0001-HEAD" $ do
    it "finds usage of 'base/head'" $
        foundHeadObservation `shouldBe` Just expectedHeadObservation
  where
    foundHeadObservation :: Maybe Observation
    foundHeadObservation = find
        (\Observation{..} -> observationId == Id "1-STAN-0001-HEAD")
        (analysisObservations analysis)

    expectedHeadObservation :: Observation
    expectedHeadObservation = Observation
        { observationId = Id "1-STAN-0001-HEAD"
        , observationInspectionId = Id "STAN-0001-HEAD"
        , observationLoc = mkRealSrcSpan
            (mkRealSrcLoc "target/Target/Example.hs" 7 12)
            (mkRealSrcLoc "target/Target/Example.hs" 7 16)
        , observationFile = "target/Target/Example.hs"
        }
