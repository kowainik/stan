{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

__Inspection__ — check or test provided by Stan.
-}

module Stan.Inspection
    ( Inspection (..)
    , InspectionAnalysis (..)
    , InspectionsMap

      -- * Pretty print
    , prettyShowInspection
    , prettyShowInspectionShort
    ) where

import Stan.Category (Category, prettyShowCategory)
import Stan.Core.Id (Id (..))
import Stan.NameMeta (NameMeta)
import Stan.Severity (Severity, prettyShowSeverity)

import Colourista (blue, bold, formatWith, green, italic)

import qualified Data.Text as T


{- | Data type that represents a check/test, or how we call it
__inspection__ that is provided by the Stan tool.
-}
data Inspection = Inspection
    { inspectionId          :: !(Id Inspection)
    , inspectionName        :: !Text
    , inspectionDescription :: !Text
    , inspectionSolution    :: ![Text]
    , inspectionCategory    :: !(NonEmpty Category)
    , inspectionSeverity    :: !Severity
    , inspectionAnalysis    :: !InspectionAnalysis
    } deriving stock (Show, Eq)

{- | Type alias for the 'HashMap' that contains pairs of inspections 'Id's and
corresponding 'Inspection's.
-}
type InspectionsMap = HashMap (Id Inspection) Inspection

{- | Data type that represents all possible types of @stan@
inspections in a uniformed way.
-}
data InspectionAnalysis
    -- | Find specific function name.
    = FindName NameMeta
    -- | Missing @infix@ declaration for operator.
    | Infix
    deriving stock (Show, Eq)

-- | Show 'Inspection' in a human-friendly format.
prettyShowInspection :: Inspection -> Text
prettyShowInspection Inspection{..} = unlines $
    [ b "~~~STAN INSPECTION~~~"
    , ""
    , i " ✲ ID:          " <> b (unId inspectionId)
    , i " ✲ Name:        " <> inspectionName
    , i " ✲ Description: " <> inspectionDescription
    , i " ✲ Severity:    " <> prettyShowSeverity inspectionSeverity
    , i " ✲ Category:    " <> T.intercalate " " (map prettyShowCategory $ toList inspectionCategory)
    , ""
    ,  formatWith [green] "Possible solutions:"
    ] <> map ("  - " <>) inspectionSolution
  where
    i, b :: Text -> Text
    i = formatWith [italic]
    b = formatWith [bold]

-- | Show the short view of a given 'Inspection'.
prettyShowInspectionShort :: Inspection -> Text
prettyShowInspectionShort Inspection{..} =
    " ❋ "
    <> formatWith [bold, blue] ("[" <> unId inspectionId <> "] ")
    <> formatWith [italic] inspectionName
