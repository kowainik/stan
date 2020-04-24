module Test.Stan.Analysis.Common
    ( observationAssert
    ) where

import FastString (FastString, mkFastString)
import SrcLoc (RealSrcSpan, mkRealSrcLoc, mkRealSrcSpan)
import System.FilePath ((</>))
import Test.Hspec (Expectation, shouldBe)

import Stan.Analysis (Analysis (..))
import Stan.Core.Id (Id (..))
import Stan.Core.ModuleName (ModuleName)
import Stan.Inspection (Inspection (..))
import Stan.Observation (Observation (..), mkObservationId)

import qualified Data.Text as Text


observationAssert
    :: FilePath  -- ^ Path to module
    -> ModuleName  -- ^ Module name
    -> Analysis
    -> Inspection
    -> Int  -- ^ Line number
    -> Int  -- ^ Span start
    -> Int  -- ^ Span end
    -> Expectation
observationAssert modulePath moduleName analysis Inspection{..} line start end =
    foundPartialObservation `shouldBe` Just expectedHeadObservation
  where
    foundPartialObservation :: Maybe Observation
    foundPartialObservation = find
        (\Observation{..} -> obsIdShort `Text.isPrefixOf` unId observationId)
        (analysisObservations analysis)

    expectedHeadObservation :: Observation
    expectedHeadObservation = Observation
        { observationId = obsId
        , observationInspectionId = inspectionId
        , observationLoc = span
        , observationFile = path
        , observationModuleName = moduleName
        , observationFileContent = maybe "" observationFileContent foundPartialObservation
        }

    obsId :: Id Observation
    obsId = mkObservationId inspectionId moduleName span

    -- Prefix of Observation Id without column number, for easier testing
    obsIdShort :: Text
    obsIdShort = Text.takeWhile (/= ':') $ unId obsId

    span :: RealSrcSpan
    span = mkRealSrcSpan
        (mkRealSrcLoc pathFS line start)
        (mkRealSrcLoc pathFS line end)

    path :: FilePath
    path = "target" </> modulePath

    pathFS :: FastString
    pathFS = mkFastString path
