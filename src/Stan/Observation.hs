{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

__Observation__ — a vulnerability found in the target project by @Stan@.
-}

module Stan.Observation
    ( Observation (..)

      -- * Smart constructors
    , mkObservationId

      -- * Pretty print
    , prettyShowObservation
    ) where

import Colourista (bold, formatWith, green, italic, reset)
import Relude.Unsafe ((!!))
import SrcLoc (RealSrcSpan, srcSpanEndCol, srcSpanStartCol, srcSpanStartLine)

import Stan.Category (prettyShowCategory)
import Stan.Core.Id (Id (..))
import Stan.Hie.Debug ()
import Stan.Inspection (Inspection (..), prettyShowSeverity, severityColour)
import Stan.Inspection.All (getInspectionById)

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

{- | Create a 'Observation' 'Id' from the numerical order and 'Inspection' 'Id'.

The 'Observation' should look like this:

@
NUM-STAN-000X-XXXX
@

where @NUM@ is the ordinal number of the 'Observation'
followed by the 'Id' of the 'Inspection'.
-}
mkObservationId :: Int -> Id Inspection -> Id Observation
mkObservationId n insId = Id $ show n <> "-" <> unId insId
