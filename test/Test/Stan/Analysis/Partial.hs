module Test.Stan.Analysis.Partial
    ( analysisHeadSpec
    ) where

import SrcLoc (mkRealSrcLoc, mkRealSrcSpan)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)

import Stan.Analysis (Analysis (..))
import Stan.Core.Id (Id (..))
import Stan.Inspection (Inspection)
import Stan.Inspection.Partial (stan0001, stan0002, stan0003, stan0004)
import Stan.Observation (Observation (..), mkObservationId)


analysisHeadSpec :: Analysis -> Spec
analysisHeadSpec analysis = describe "Partial functions" $ do
    it "STAN-0001: finds usage of 'base/head'" $
        foundObservation analysis 1 stan0001 7 12 16
    it "STAN-0002: finds usage of 'base/tail'" $
        foundObservation analysis 1 stan0002 10 12 16
    it "STAN-0003: finds usage of 'base/init'" $
        foundObservation analysis 1 stan0003 13 12 16
    it "STAN-0004: finds usage of 'base/last'" $
        foundObservation analysis 1 stan0004 16 12 16

foundObservation
    :: Analysis
    -> Int  -- ^ 'Observation' number
    -> Id Inspection
    -> Int  -- ^ Line number
    -> Int  -- ^ Span start
    -> Int  -- ^ Span end
    -> Expectation
foundObservation analysis num insId line start end =
    foundPartialObservation `shouldBe` Just expectedHeadObservation
  where
    foundPartialObservation :: Maybe Observation
    foundPartialObservation = find
        (\Observation{..} -> observationId == obsId)
        (analysisObservations analysis)

    expectedHeadObservation :: Observation
    expectedHeadObservation = Observation
        { observationId = obsId
        , observationInspectionId = insId
        , observationLoc = mkRealSrcSpan
            (mkRealSrcLoc "target/Target/Example.hs" line start)
            (mkRealSrcLoc "target/Target/Example.hs" line end)
        , observationFile = "target/Target/Example.hs"
        , observationModuleName = "Target.Example"
        , observationFileContent = maybe "" observationFileContent foundPartialObservation
        }

    obsId :: Id Observation
    obsId = mkObservationId num insId
