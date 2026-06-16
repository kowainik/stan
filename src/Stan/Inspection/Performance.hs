{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Contains all 'Inspection's for known performance improvements.

The __preformance__ inspections are in ranges:

* @STAN-0401 .. STAN-0500@

-}

module Stan.Inspection.Performance
    ( -- * Performance inspections
      -- *** @SPECIALIZE@ pragma
      stan0401

      -- * All inspections
    , performanceInspectionsMap
    ) where

import Relude.Extra.Tuple (fmapToFst)

import Stan.Core.Id (Id (..))
import Stan.Inspection (Inspection (..), InspectionAnalysis (..), InspectionsMap)
import Stan.Severity (Severity (..))

import qualified Stan.Category as Category


-- | All performance 'Inspection's map from 'Id's.
performanceInspectionsMap :: InspectionsMap
performanceInspectionsMap = fromList $ fmapToFst inspectionId
    [ stan0401
    ]


-- | 'Inspection' — @SPECIALIZE@ @STAN-0401@.
stan0401 :: Inspection
stan0401 = Inspection
    { inspectionId = Id "STAN-0401"
    , inspectionName = "Performance: SPECIALIZE pragma"
    , inspectionDescription = "Use {-# SPECIALIZE #-} pragma to improve performance"
    , inspectionSolution = []
    , inspectionCategory = Category.antiPattern :| []
    , inspectionSeverity = Performance
    , inspectionAnalysis = SpecializePragma
    }
