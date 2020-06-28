{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

__Observation__ — a vulnerability found in the target project by @Stan@.
-}

module Stan.Observation
    ( Observation (..)
    , Observations

      -- * Smart constructors
    , mkObservation
    , mkObservationId

    , ignoredObservations

      -- * Pretty print
    , prettyShowObservation
    , prettyShowIgnoredObservations
    , prettyObservationSource
    ) where

import Colourista (blue, bold, formatWith, green, italic, reset, yellow)
import Colourista.Short (b, i)
import Data.List (partition)
import Slist (Slist)

import Stan.Category (prettyShowCategory)
import Stan.Core.Id (Id (..))
import Stan.Core.ModuleName (ModuleName (..), fromGhcModule)
import Stan.Ghc.Compat (RealSrcSpan, srcSpanEndCol, srcSpanEndLine, srcSpanFile, srcSpanStartCol,
                        srcSpanStartLine)
import Stan.Hie.Compat (HieFile (..))
import Stan.Inspection (Inspection (..))
import Stan.Inspection.All (getInspectionById)
import Stan.Report.Settings (ReportSettings (..), Verbosity (..), isHidden)
import Stan.Severity (prettyShowSeverity, severityColour)

import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Text
import qualified Slist as S


{- | Data type to represent discovered by Stan vulnerabilities.
-}
data Observation = Observation
    { observationId           :: !(Id Observation)
    , observationInspectionId :: !(Id Inspection)
    , observationLoc          :: !RealSrcSpan
    , observationFile         :: !FilePath
    , observationModuleName   :: !ModuleName
    , observationFileContent  :: !ByteString
    } deriving stock (Show, Eq)

-- | Type alias for the sized list of 'Observation's.
type Observations = Slist Observation

-- | Smart constructor for 'Observation's from 'HieFile's.
mkObservation
    :: Id Inspection  -- ^ Corresponding 'Inspection's 'Id'.
    -> HieFile
    -> RealSrcSpan  -- ^ Position.
    -> Observation
mkObservation insId HieFile{..} srcSpan = Observation
    { observationId = mkObservationId insId moduleName srcSpan
    , observationInspectionId = insId
    , observationLoc = srcSpan
    , observationFile = hie_hs_file
    , observationModuleName = moduleName
    , observationFileContent = hie_hs_src
    }
  where
    moduleName :: ModuleName
    moduleName = fromGhcModule hie_module


-- | Show 'Observation' in a human-friendly format.
prettyShowObservation :: ReportSettings -> Observation -> Text
prettyShowObservation ReportSettings{..} o@Observation{..} = case reportSettingsVerbosity of
    NonVerbose -> simpleShowObservation
    Verbose -> unlines $ map (" ┃  " <>)
        $  observationTable
        <> ("" : prettyObservationSource True o)
        <> ("" : solution)
  where
    simpleShowObservation :: Text
    simpleShowObservation =
        " ✦ "
        <> b (unId observationId)
        <>" [" <> sev <> "] "
        <> i (showSpan observationLoc)
        <> " — "
        <> inspectionName inspection

    showSpan :: RealSrcSpan -> Text
    showSpan s = show (srcSpanFile s)
        <> "(" <> show (srcSpanStartLine s)
        <> ":" <> show (srcSpanStartCol s)
        <> "-" <> show (srcSpanEndLine s)
        <> ":" <> show (srcSpanEndCol s)
        <> ")"

    observationTable :: [Text]
    observationTable =
        [ element "ID:            " <> b (unId observationId)
        , element "Severity:      " <> sev
        , element "Inspection ID: " <> unId observationInspectionId
        , element "Name:          " <> inspectionName inspection
        , element "Description:   " <> inspectionDescription inspection
        , element "Category:      " <> categories
        , element "File:          " <> toText observationFile
        ]
      where
        element :: Text -> Text
        element = formatWith [italic] . ("✦ " <>)

    sev :: Text
    sev = prettyShowSeverity (inspectionSeverity inspection)

    inspection :: Inspection
    inspection = getInspectionById observationInspectionId

    categories :: Text
    categories = Text.intercalate " "
        $ map prettyShowCategory $ NE.toList $ inspectionCategory inspection

    solution :: [Text]
    solution
        | isHidden reportSettingsSolutionVerbosity || null sols = []
        | otherwise = "💡 " <> formatWith [italic, green] "Possible solution:" :
            map ("    ⍟ " <>) sols
      where
        sols = inspectionSolution inspection


prettyObservationSource
    :: Bool  -- ^ Use colouring
    -> Observation
    -> [Text]
prettyObservationSource isColour Observation{..} =
      alignLine (n - 1)
    : map (\x -> alignLine x <> getSourceLine x) [n .. endL]
    ++ [alignLine (endL + 1) <> arrows]
  where
    n, endL :: Int
    n = srcSpanStartLine observationLoc
    endL = srcSpanEndLine observationLoc

    alignLine :: Int -> Text
    alignLine x = Text.justifyRight 4 ' ' (show x) <> " ┃ "

    getSourceLine :: Int -> Text
    getSourceLine x = maybe
        "<UNAVAILABLE> Open the issue in the tool that created the HIE files for you."
        decodeUtf8
        (BS.lines observationFileContent !!? (x - 1))

    arrows :: Text
    arrows = whenColour isColour (severityColour $ inspectionSeverity $ getInspectionById observationInspectionId)
        <> Text.replicate start " "
        <> Text.replicate arrow "^"
        <> whenColour isColour reset
      where
        start = srcSpanStartCol observationLoc - 1
        arrow = srcSpanEndCol observationLoc - start - 1

{- | Checkes the predicate on colourfulness and returns an empty text when the
colouroing is disabled.
-}
whenColour :: Bool -> Text -> Text
whenColour = memptyIfFalse

{- Returns the list of ignored and unrecognised 'Observation' 'Id's
respectfully.
-}
ignoredObservations
    :: [Id Observation]
    -> Observations
    -> ([Id Observation], [Id Observation])
      -- ^ Ignored         ^ Unknown
ignoredObservations ids obs = (ignoredIds, unknownIds)
  where
    obsIds :: HashSet (Id Observation)
    obsIds = fromList $ toList $ S.map observationId obs

    ignoredIds, unknownIds :: [Id Observation]
    (ignoredIds, unknownIds) = partition (`HS.member` obsIds) ids

{- Pretty shows the list of ignored and unrecognised 'Observation' 'Id's
respectfully.

@
Ignored Observation IDs:
    - OBS-STAN-0005-ZKmeC0-125:45
Unrecognised Observation IDs:
    - asd
@
-}
prettyShowIgnoredObservations :: [Id Observation] -> Observations -> Text
prettyShowIgnoredObservations [] _ = ""
prettyShowIgnoredObservations ids obs = ignored <> unknown
  where
    ignored :: Text
    ignored =
        if null ignoredIds
        then ""
        else formatWith [bold, blue] "Ignored Observation IDs:\n"
            <> showIds ignoredIds

    unknown :: Text
    unknown =
        if null unknownIds
        then ""
        else formatWith [bold, yellow] "Unrecognised Observation IDs:\n"
            <> showIds unknownIds

    showIds :: [Id Observation] -> Text
    showIds = unlines . map ((<>) "    - " . unId)

    ignoredIds, unknownIds :: [Id Observation]
    (ignoredIds, unknownIds) = ignoredObservations ids obs

{- | Create a stable 'Observation' 'Id' in a such way that:

1. 'Id' doesn't depend on other inspections in this file.
2. 'Id' uniquely identifies 'Observation' location.
3. 'Id's are guaranteed to be the same if the module content didn't
change between different @stan@ runs.

The 'Observation' 'Id' should look like this:

@
OBS-STAN-XXXX-<module-name-hash>-10:42
@
-}
mkObservationId :: Id Inspection -> ModuleName -> RealSrcSpan -> Id Observation
mkObservationId insId moduleName srcSpan = Id $ Text.intercalate "-"
    [ "OBS"
    , unId insId
    , hashModuleName moduleName
    , show (srcSpanStartLine srcSpan) <> ":" <> show (srcSpanStartCol srcSpan)
    ]

{- | Hash module name to a short string of length @6@. Hashing
algorithm is the following:

1. First, run SHA-1.
2. Then, encode with @base64@.
3. Last, take first @6@ characters.
-}
hashModuleName :: ModuleName -> Text
hashModuleName =
    Text.take 6
    . Base64.encodeBase64
    . SHA1.hash
    . encodeUtf8
    . unModuleName
