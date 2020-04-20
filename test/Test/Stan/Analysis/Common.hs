module Test.Stan.Analysis.Common
    ( observationAssert
    ) where

import FastString (FastString, mkFastString)
import SrcLoc (RealSrcSpan, mkRealSrcLoc, mkRealSrcSpan)
import System.FilePath ((</>))
import Test.Hspec (Expectation, shouldBe)

import Stan.Analysis (Analysis (..))
import Stan.Core.Id (Id (..))
import Stan.Inspection (Inspection)
import Stan.Observation (Observation (..), mkObservationId)


observationAssert
    :: FilePath  -- ^ Path to module
    -> Text  -- ^ Module name
    -> Analysis
    -> Id Inspection
    -> Int  -- ^ Line number
    -> Int  -- ^ Span start
    -> Int  -- ^ Span end
    -> Expectation
observationAssert modulePath moduleName analysis insId line start end =
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
        , observationLoc = span
        , observationFile = path
        , observationModuleName = moduleName
        , observationFileContent = maybe "" observationFileContent foundPartialObservation
        }

    obsId :: Id Observation
    obsId = mkObservationId insId moduleName span

    span :: RealSrcSpan
    span = mkRealSrcSpan
        (mkRealSrcLoc pathFS line start)
        (mkRealSrcLoc pathFS line end)

    path :: FilePath
    path = "target" </> modulePath

    pathFS :: FastString
    pathFS = mkFastString path
