{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

__Observation__ — a vulnerability found in the target project by @Stan@.
-}

module Stan.Observation
    ( Observation (..)

      -- * Smart constructors
    , mkObservation
    , mkObservationId

      -- * Pretty print
    , prettyShowObservation
    ) where

import Colourista (bold, formatWith, green, italic, reset)
import HieTypes (HieFile (..))
import Module (moduleName, moduleNameString)
import Relude.Unsafe ((!!))
import SrcLoc (RealSrcSpan, srcSpanEndCol, srcSpanStartCol, srcSpanStartLine)

import Stan.Category (prettyShowCategory)
import Stan.Core.Id (Id (..))
import Stan.Hie.Debug ()
import Stan.Inspection (Inspection (..))
import Stan.Inspection.All (getInspectionById)
import Stan.Severity (prettyShowSeverity, severityColour)

import qualified Data.ByteString.Char8 as BS
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Text


{- | Data type to represent discovered by Stan vulnerabilities.
-}
data Observation = Observation
    { observationId           :: !(Id Observation)
    , observationInspectionId :: !(Id Inspection)
    , observationLoc          :: !RealSrcSpan
    , observationFile         :: !FilePath
    , observationModuleName   :: !Text
    , observationFileContent  :: !ByteString
    } deriving stock (Show, Eq)

-- | Smart constructor for 'Observation's from 'HieFile's.
mkObservation
    :: Id Inspection  -- ^ Corresponding 'Inspection's 'Id'.
    -> HieFile
    -> RealSrcSpan  -- ^ Position.
    -> Observation
mkObservation insId HieFile{..} srcSpan = Observation
    { observationId = mkObservationId insId modName srcSpan
    , observationInspectionId = insId
    , observationLoc = srcSpan
    , observationFile = hie_hs_file
    , observationModuleName = modName
    , observationFileContent = hie_hs_src
    }
  where
    modName :: Text
    modName = toText $ moduleNameString $ moduleName hie_module

-- | Show 'Observation' in a human-friendly format.
prettyShowObservation :: Observation -> Text
prettyShowObservation Observation{..} = unlines $
    map (" ┃  " <>)
        $  observationTable
        <> ("" : source)
        <> ("" : solution)
  where
    observationTable :: [Text]
    observationTable =
        [ element "ID:            " <> b (unId observationId)
        , element "Severity:      " <> prettyShowSeverity (inspectionSeverity inspection)
        , element "Description:   " <> inspectionDescription inspection
        , element "Inspection ID: " <> unId observationInspectionId
        , element "Category:      " <> categories
        , element "File:          " <> toText observationFile
        ]
      where
        element :: Text -> Text
        element = formatWith [italic] . ("✦ " <>)

        b :: Text -> Text
        b = formatWith [bold]

    inspection :: Inspection
    inspection = getInspectionById observationInspectionId

    categories :: Text
    categories = Text.intercalate " "
        $ map prettyShowCategory $ NE.toList $ inspectionCategory inspection

    source :: [Text]
    source =
        [ alignLine (n - 1)
        , alignLine n <> getSourceLine
        , alignLine (n + 1) <> arrows
        ]
      where
        n :: Int
        n = srcSpanStartLine observationLoc

        alignLine :: Int -> Text
        alignLine x = Text.justifyRight 4 ' ' (show x) <> " ┃ "

        getSourceLine :: Text
        getSourceLine = decodeUtf8 $
            BS.lines observationFileContent !! (n - 1)

        arrows :: Text
        arrows = severityColour (inspectionSeverity inspection)
            <> Text.replicate start " "
            <> Text.replicate arrow "^"
            <> reset
          where
            start = srcSpanStartCol observationLoc - 1
            arrow = srcSpanEndCol observationLoc - start - 1

    solution :: [Text]
    solution = case inspectionSolution inspection of
        []   -> []
        sols -> formatWith [italic, green] "Possible solution:" :
            map (" ⍟ " <>) sols

{- | Create a 'Observation' 'Id' in a such way that:

1. 'Id' is stable across multiple runs of @stan@ and doesn't depend on
other inspections in this file.
2. 'Id' uniquely identifies 'Observation' location.

The 'Observation' 'Id' should look like this:

@
STAN-XXXX-Some.Module.Name-line10-column45
@
-}
mkObservationId :: Id Inspection -> Text -> RealSrcSpan -> Id Observation
mkObservationId insId modName srcSpan = Id $ Text.intercalate "-"
    [ unId insId
    , modName
    , "line" <> show (srcSpanStartLine srcSpan)
    , "column" <> show (srcSpanStartCol srcSpan)
    ]
