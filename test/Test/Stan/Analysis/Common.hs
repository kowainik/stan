module Test.Stan.Analysis.Common
    ( observationSpec
    ) where

import FastString (FastString, mkFastString)
import SrcLoc (mkRealSrcLoc, mkRealSrcSpan)
import System.FilePath ((</>))
import Test.Hspec (Expectation, shouldBe)

import Stan.Analysis (Analysis (..))
import Stan.Core.Id (Id (..))
import Stan.Inspection (Inspection)
import Stan.Observation (Observation (..), mkObservationId)


observationSpec
    :: FilePath  -- ^ Path to module
    -> Text  -- ^ Module name
    -> Int  -- ^ 'Observation' number
    -> Analysis
    -> Id Inspection
    -> Int  -- ^ Line number
    -> Int  -- ^ Span start
    -> Int  -- ^ Span end
    -> Expectation
observationSpec modulePath moduleName num analysis insId line start end =
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
            (mkRealSrcLoc pathFS line start)
            (mkRealSrcLoc pathFS line end)
        , observationFile = path
        , observationModuleName = moduleName
        , observationFileContent = maybe "" observationFileContent foundPartialObservation
        }

    obsId :: Id Observation
    obsId = mkObservationId num insId

    path :: FilePath
    path = "target" </> modulePath

    pathFS :: FastString
    pathFS = mkFastString path
