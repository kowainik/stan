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
    , stan0101Inspection
    , stan0101Meta
      -- *** Infinite 'Data.OldList.isSuffixOf'
    , stan0102
    , stan0102Inspection
    , stan0102Meta
      -- *** Infinite 'Data.OldList.genericLength'
    , stan0103
    , stan0103Inspection
    , stan0103Meta

      -- * All inspections
    , infiniteInspectionsMap
    ) where

import Relude.Extra.Tuple (mapToFst)

import Stan.Category (infinite)
import Stan.Core.Id (Id (..))
import Stan.Inspection (Inspection (..), InspectionsMap)
import Stan.NameMeta (NameMeta (..), mkBaseListMeta)
import Stan.Severity (Severity (..))


-- | All infinite 'Inspection's map from 'Id's.
infiniteInspectionsMap :: InspectionsMap
infiniteInspectionsMap = fromList $ map (mapToFst inspectionId)
    [ stan0101Inspection
    , stan0102Inspection
    , stan0103Inspection
    ]

-- | Smart constructor to create infinite 'Inspection'.
mkInfiniteInspection :: Id Inspection -> NameMeta -> Inspection
mkInfiniteInspection insId NameMeta{..} = Inspection
    { inspectionId = insId
    , inspectionName = "Infinite: " <> nameMetaPackage <> "/" <> nameMetaName
    , inspectionDescription =
        "Usage of the '" <> nameMetaName <> "' function that hangs on infinite lists"
    , inspectionSolution = []
    , inspectionCategory = one infinite
    , inspectionSeverity = Warning
    }

-- | 'Id' for the infinite 'reverse' 'Inspection' — @STAN-0101@.
stan0101 :: Id Inspection
stan0101 = Id "STAN-0101"

-- | Corresponding 'Inspection' for 'stan0101' — infinite 'reverse' @STAN-0101@.
stan0101Inspection :: Inspection
stan0101Inspection = mkInfiniteInspection stan0101 stan0101Meta

-- | Corresponding 'NameMeta' for 'stan0101' — infinite 'reverse' @STAN-0101@.
stan0101Meta :: NameMeta
stan0101Meta = mkBaseListMeta "reverse"

-- | 'Id' for the infinite 'Data.OldList.isSuffixOf' 'Inspection' — @STAN-0102@.
stan0102 :: Id Inspection
stan0102 = Id "STAN-0102"

-- | Corresponding 'Inspection' for 'stan0102' — infinite 'Data.OldList.isSuffixOf' @STAN-0102@.
stan0102Inspection :: Inspection
stan0102Inspection = mkInfiniteInspection stan0102 stan0102Meta

-- | Corresponding 'NameMeta' for 'stan0102' — infinite 'Data.OldList.isSuffixOf' @STAN-0102@.
stan0102Meta :: NameMeta
stan0102Meta = (mkBaseListMeta "isSuffixOf")
    { nameMetaModuleName = "Data.OldList"
    }

-- | 'Id' for the infinite 'Data.OldList.genericLength' 'Inspection' — @STAN-0103@.
stan0103 :: Id Inspection
stan0103 = Id "STAN-0103"

{- | Corresponding 'Inspection' for 'stan0103' — infinite
'Data.OldList.genericLength' @STAN-0103@.
-}
stan0103Inspection :: Inspection
stan0103Inspection = mkInfiniteInspection stan0103 stan0103Meta

{- | Corresponding 'NameMeta' for 'stan0103' — infinite
'Data.OldList.genericLength' @STAN-0103@.
-}
stan0103Meta :: NameMeta
stan0103Meta = (mkBaseListMeta "genericLength")
    { nameMetaModuleName = "Data.OldList"
    }
