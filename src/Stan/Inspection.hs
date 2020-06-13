{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

__Inspection__ — check or test provided by Stan.
-}

module Stan.Inspection
    ( -- * Stan inspection type
      Inspection (..)
    , categoryL
    , descriptionL
    , solutionL
    , severityL
    , analysisL

      -- * Inspection info
    , InspectionAnalysis (..)
    , InspectionsMap

      -- * Sorting
    , sortById
      -- * Pretty print
    , prettyShowInspection
    , prettyShowInspectionShort
      -- ** Markdown
    , inspectionsMd
    ) where

import Relude.Extra.Lens (Lens', lens)

import Colourista (blue, bold, formatWith, green)
import Colourista.Short (b, i)

import Stan.Category (Category (..), prettyShowCategory)
import Stan.Core.Id (Id (..))
import Stan.Pattern.Ast (PatternAst)
import Stan.Severity (Severity, prettyShowSeverity)

import qualified Data.HashMap.Strict as HM
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

descriptionL :: Lens' Inspection Text
descriptionL = lens
    inspectionDescription
    (\inspection new -> inspection { inspectionDescription = new })

solutionL :: Lens' Inspection [Text]
solutionL = lens
    inspectionSolution
    (\inspection new -> inspection { inspectionSolution = new })

categoryL :: Lens' Inspection (NonEmpty Category)
categoryL = lens
    inspectionCategory
    (\inspection new -> inspection { inspectionCategory = new })

severityL :: Lens' Inspection Severity
severityL = lens
    inspectionSeverity
    (\inspection new -> inspection { inspectionSeverity = new })

analysisL :: Lens' Inspection InspectionAnalysis
analysisL = lens
    inspectionAnalysis
    (\inspection new -> inspection { inspectionAnalysis = new })

{- | Type alias for the 'HashMap' that contains pairs of inspections 'Id's and
corresponding 'Inspection's.
-}
type InspectionsMap = HashMap (Id Inspection) Inspection

-- | Sort 'Inspection' by 'Id'
sortById :: InspectionsMap -> [Inspection]
sortById = sortWith inspectionId . HM.elems

{- | Data type that represents all possible types of @stan@
inspections in a uniformed way.
-}
data InspectionAnalysis
    -- | Find the specific part of the Haskell AST (including specific functions).
    = FindAst !PatternAst
    -- | Find all operators without matching @infix[r|l]@
    | Infix
    -- | Check if the data type has lazy fields
    | LazyField
    -- | Usage of tuples with size >= 4
    | BigTuples
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

-- | Show the short view of a given 'Inspection'.
prettyShowInspectionShort :: Inspection -> Text
prettyShowInspectionShort Inspection{..} =
    " ❋ "
    <> formatWith [bold, blue] ("[" <> unId inspectionId <> "] ")
    <> i inspectionName

{- | Create the MarkDown text for all inspections.
The generated MD has a ToC and separate sections for each inspection.

This is used to keep the Wiki page of the project up to date.
-}
inspectionsMd :: [Inspection] -> Text
inspectionsMd inss = intro <> toc <> unlines (map inspectionToMd inss)
  where
    intro :: Text
    intro = "This document contains information about all inspections used in Stan to find observations in your projects. Below you can see more details about each inspection individually\n\n"

    toc :: Text
    toc = "## Table of all Inspections\n\n" <> unlines (map insLink inss) <> "\n"

    insLink :: Inspection -> Text
    insLink (unId . inspectionId -> ins)= " * [" <> ins <> "](#" <> ins <> ")"

inspectionToMd :: Inspection -> Text
inspectionToMd Inspection{..} = unlines $
    [ "## " <> unId inspectionId
    , ""
    , "[[Back to the Table of all Inspections] ↑](#table-of-all-inspections)"
    , ""
    , "| Property | Value |"
    , "|--|--|"
    , "| ID          | " <> unId inspectionId <> " |"
    , "| Name        | " <> inspectionName <> " |"
    , "| Description | " <> inspectionDescription <> " |"
    , "| Severity    | " <> show inspectionSeverity <> " |"
    , "| Category    | " <> "#" <> T.intercalate " #" (map unCategory $ toList inspectionCategory) <> " |"
    , ""
    , "#### Possible solutions for " <> unId inspectionId
    ] <> map ("  - " <>) inspectionSolution
