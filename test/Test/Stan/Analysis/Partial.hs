module Test.Stan.Analysis.Partial
    ( analysisHeadSpec
    ) where

import SrcLoc (mkRealSrcLoc, mkRealSrcSpan)
import Test.Hspec (Spec, describe, it, shouldBe)

import Stan.Analysis (Analysis (..))
import Stan.Core.Id (Id (..))
import Stan.Inspection (stan0001)
import Stan.Observation (Observation (..), mkObservationId)


analysisHeadSpec :: Analysis -> Spec
analysisHeadSpec analysis = describe "STAN-0001" $ do
    it "finds usage of 'base/head'" $
        foundHeadObservation `shouldBe` Just expectedHeadObservation
  where
    foundHeadObservation :: Maybe Observation
    foundHeadObservation = find
        (\Observation{..} -> observationId == obsId1)
        (analysisObservations analysis)

    expectedHeadObservation :: Observation
    expectedHeadObservation = Observation
        { observationId = obsId1
        , observationInspectionId = stan0001
        , observationLoc = mkRealSrcSpan
            (mkRealSrcLoc "target/Target/Example.hs" 7 12)
            (mkRealSrcLoc "target/Target/Example.hs" 7 16)
        , observationFile = "target/Target/Example.hs"
        , observationModuleName = "Target.Example"
        , observationFileContent = maybe "" observationFileContent foundHeadObservation
        }

    obsId1 :: Id Observation
    obsId1 = mkObservationId 1 stan0001
