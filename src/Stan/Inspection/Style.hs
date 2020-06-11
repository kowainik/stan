{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Contains all 'Inspection's for style improvements.

The __style__ inspections are in ranges:

* @STAN-0301 .. STAN-0400@
-}

module Stan.Inspection.Style
    ( -- * Style inspections
      -- *** Missing fixity
      stan0301
      -- *** Too big tuples
    , stan0302

      -- * All inspections
    , styleInspectionsMap
    ) where

import Relude.Extra.Tuple (fmapToFst)

import Stan.Core.Id (Id (..))
import Stan.Inspection (Inspection (..), InspectionAnalysis (..), InspectionsMap)
import Stan.Severity (Severity (Style))

import qualified Stan.Category as Category


-- | All anti-pattern 'Inspection's map from 'Id's.
styleInspectionsMap :: InspectionsMap
styleInspectionsMap = fromList $ fmapToFst inspectionId
    [ stan0301
    , stan0302
    ]

-- | 'Inspection' — missing fixity declaration @STAN-0301@.
stan0301 :: Inspection
stan0301 = Inspection
    { inspectionId = Id "STAN-0301"
    , inspectionName = "Missing fixity declaration for operator"
    , inspectionDescription = "Using the implicit default fixity for operator: infixl 9"
    , inspectionSolution =
        [ "Add 'infix[l|r]' declaration to the operator with explicit precedence"
        ]
    , inspectionCategory = Category.syntax :| []
    , inspectionSeverity = Style
    , inspectionAnalysis = Infix
    }

-- | 'Inspection' — to big tuples @STAN-0302@.
stan0302 :: Inspection
stan0302 = Inspection
    { inspectionId = Id "STAN-0302"
    , inspectionName = "Big tuples"
    , inspectionDescription =
        "Using tuples of big size (>= 4) can decrease code readability"
    , inspectionSolution =
        [ "Consider defining and using a custom data type to improve code comprehension"
        ]
    , inspectionCategory = Category.antiPattern :| [Category.syntax]
    , inspectionSeverity = Style
    , inspectionAnalysis = BigTuples
    }
