{- HLINT ignore "Eta reduce" -}

module Test.Stan.Analysis.Common
    ( observationAssert
    , observationAssertMulti
    , noObservationAssert
    , itShouldStr
    , unsafeNameMeta
    ) where

import FastString (FastString, mkFastString)
import SrcLoc (RealSrcSpan, mkRealSrcLoc, mkRealSrcSpan, srcSpanStartLine)
import System.FilePath (pathSeparator, (</>))
import Test.Hspec (Expectation, shouldBe)

import Stan.Analysis (Analysis (..))
import Stan.Core.Id (Id (..))
import Stan.Core.ModuleName (ModuleName (..))
import Stan.Inspection (Inspection (..), InspectionAnalysis (..))
import Stan.NameMeta (NameMeta, prettyShowNameMeta)
import Stan.Observation (Observation (..), mkObservationId)
import Stan.Pattern.Ast (PatternAst (..))

import qualified Data.Text as Text


{- | Checks that there's 'Observation' of a given inspection in a
given line and span.
-}
observationAssert
    :: [String] -- ^ Module name
    -> Analysis
    -> Inspection
    -> Int  -- ^ Line number
    -> Int  -- ^ Span start
    -> Int  -- ^ Span end
    -> Expectation
observationAssert moduleParts analysis inspection line start end =
    observationAssertMulti moduleParts analysis inspection line start line end

{- | Checks that there's 'Observation' of a given inspection in a
given line and span.
-}
observationAssertMulti
    :: [String]  -- ^ Module name parts
    -> Analysis
    -> Inspection
    -> Int  -- ^ Start Line number
    -> Int  -- ^ Span start
    -> Int  -- ^ End Line number
    -> Int  -- ^ Span end
    -> Expectation
observationAssertMulti
    moduleParts
    analysis
    Inspection{..}
    firstLine
    start
    lastLine
    end
  =
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
        , observationSrcSpan = span
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
        (mkRealSrcLoc pathFS firstLine start)
        (mkRealSrcLoc pathFS lastLine end)

    path :: FilePath
    path = filePathFromParts moduleParts

    moduleName :: ModuleName
    moduleName = moduleFromParts moduleParts

    pathFS :: FastString
    pathFS = mkFastString path

-- | Checks that there's no 'Observation' of a given inspection in a given line.
noObservationAssert
    :: [String]  -- ^ Module parts
    -> Analysis
    -> Inspection
    -> Int  -- ^ Line number
    -> Expectation
noObservationAssert parts analysis Inspection{..} line =
    foundPartialObservation `shouldBe` Nothing
  where
    foundPartialObservation :: Maybe Observation
    foundPartialObservation = find
        (\Observation{..} ->
            observationInspectionId == inspectionId
            && observationFile == filePathFromParts parts
            && observationModuleName == moduleFromParts parts
            && srcSpanStartLine observationSrcSpan == line
        )
        (analysisObservations analysis)

filePathFromParts :: [String] -> FilePath
filePathFromParts parts =
    "target" </> "Target" </> intercalate [pathSeparator] parts <> ".hs"

moduleFromParts :: [String] -> ModuleName
moduleFromParts parts = ModuleName $ toText $ "Target." <> intercalate "." parts

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
