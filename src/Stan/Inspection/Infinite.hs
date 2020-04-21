{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Contains all 'Inspection's for functions that hang on infinite lists.
-}

module Stan.Inspection.Infinite
    ( -- * Infinite inspections
      -- *** Infinite 'GHC.List.reverse'
      stan0101
      -- *** Infinite 'Data.OldList.isSuffixOf'
    , stan0102
      -- *** Infinite 'Data.OldList.genericLength'
    , stan0103

      -- * All inspections
    , infiniteInspectionsMap
    ) where

import Relude.Extra.Tuple (mapToFst)

import Stan.Category (infinite)
import Stan.Core.Id (Id (..))
import Stan.Inspection (Inspection (..), InspectionAnalysis (..), InspectionsMap)
import Stan.NameMeta (NameMeta (..), mkBaseListMeta)
import Stan.Severity (Severity (..))


-- | All infinite 'Inspection's map from 'Id's.
infiniteInspectionsMap :: InspectionsMap
infiniteInspectionsMap = fromList $ map (mapToFst inspectionId)
    [ stan0101
    , stan0102
    , stan0103
    ]

-- | Smart constructor to create infinite 'Inspection'.
mkInfiniteInspection :: Id Inspection -> NameMeta -> Inspection
mkInfiniteInspection insId nameMeta@NameMeta{..} = Inspection
    { inspectionId = insId
    , inspectionName = "Infinite: " <> nameMetaPackage <> "/" <> nameMetaName
    , inspectionDescription =
        "Usage of the '" <> nameMetaName <> "' function that hangs on infinite lists"
    , inspectionSolution = []
    , inspectionCategory = one infinite
    , inspectionSeverity = Warning
    , inspectionAnalysis = FindName nameMeta
    }

-- | 'Inspection' for 'stan0101' — infinite 'reverse' @STAN-0101@.
stan0101 :: Inspection
stan0101 = mkInfiniteInspection (Id "STAN-0101") (mkBaseListMeta "reverse")

-- | 'Inspection' for 'stan0102' — infinite 'Data.OldList.isSuffixOf' @STAN-0102@.
stan0102 :: Inspection
stan0102 = mkInfiniteInspection
    (Id "STAN-0102")
    (mkBaseListMeta "isSuffixOf") { nameMetaModuleName = "Data.OldList" }

-- | 'Inspection' for 'stan0103' — infinite 'Data.OldList.genericLength' @STAN-0103@.
stan0103 :: Inspection
stan0103 = mkInfiniteInspection
    (Id "STAN-0103")
    (mkBaseListMeta "genericLength") { nameMetaModuleName = "Data.OldList" }
