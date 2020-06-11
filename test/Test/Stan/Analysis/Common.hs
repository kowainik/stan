module Test.Stan.Analysis.Common
    ( observationAssert
    , noObservationAssert
    , itShouldStr
    , unsafeNameMeta
    ) where

import FastString (FastString, mkFastString)
import SrcLoc (RealSrcSpan, mkRealSrcLoc, mkRealSrcSpan, srcSpanStartLine)
import System.FilePath ((</>))
import Test.Hspec (Expectation, shouldBe)

import Stan.Analysis (Analysis (..))
import Stan.Core.Id (Id (..))
import Stan.Core.ModuleName (ModuleName)
import Stan.Inspection (Inspection (..), InspectionAnalysis (..))
import Stan.NameMeta (NameMeta, prettyShowNameMeta)
import Stan.Observation (Observation (..), mkObservationId)
import Stan.Pattern.Ast (PatternAst (..))

import qualified Data.Text as Text


{- | Checks that there's 'Observation' of a given inspection in a
given line and span.
-}
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

-- | Checks that there's no 'Observation' of a given inspection in a given line.
noObservationAssert
    :: FilePath  -- ^ Path to module
    -> ModuleName  -- ^ Module name
    -> Analysis
    -> Inspection
    -> Int  -- ^ Line number
    -> Expectation
noObservationAssert modulePath moduleName analysis Inspection{..} line =
    foundPartialObservation `shouldBe` Nothing
  where
    foundPartialObservation :: Maybe Observation
    foundPartialObservation = find
        (\Observation{..} ->
            observationInspectionId == inspectionId
            && observationFile == "target" </> modulePath
            && observationModuleName == moduleName
            && srcSpanStartLine observationLoc == line
        )
        (analysisObservations analysis)

-- | Generates text to be used in tests names.
itShouldStr :: Inspection -> String
itShouldStr Inspection{..} = toString $ unId inspectionId
    <> ": finds usage of '"
    <> prettyShowNameMeta (unsafeNameMeta inspectionAnalysis)
    <> "'"

unsafeNameMeta :: InspectionAnalysis -> NameMeta
unsafeNameMeta (FindAst (PatternAstName nm _)) = nm
unsafeNameMeta (FindAst (PatternAstOr (PatternAstName nm _) _)) = nm
unsafeNameMeta _ = error "Impossible happened in tests"
