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
      -- *** Infinite 'Data.Foldable.length'
    , stan0103
      -- *** Infinite 'Data.OldList.genericLength'
    , stan0104
      -- *** Infinite 'Data.Foldable.sum'
    , stan0105
      -- *** Infinite 'Data.Foldable.product'
    , stan0106

      -- * All inspections
    , infiniteInspectionsMap
    ) where

import Relude.Extra.Tuple (mapToFst)

import Stan.Core.Id (Id (..))
import Stan.Hie.Match (Pattern (..), patternList)
import Stan.Inspection (Inspection (..), InspectionAnalysis (..), InspectionsMap)
import Stan.NameMeta (NameMeta (..), mkBaseFoldableMeta, mkBaseListMeta, mkBaseOldListMeta)
import Stan.Severity (Severity (..))

import qualified Stan.Category as Category


-- | All infinite 'Inspection's map from 'Id's.
infiniteInspectionsMap :: InspectionsMap
infiniteInspectionsMap = fromList $ map (mapToFst inspectionId)
    [ stan0101
    , stan0102
    , stan0103
    , stan0104
    , stan0105
    , stan0106
    ]

-- | Smart constructor to create infinite 'Inspection'.
mkInfiniteInspection :: Id Inspection -> NameMeta -> Pattern -> Inspection
mkInfiniteInspection insId nameMeta@NameMeta{..} pat = Inspection
    { inspectionId = insId
    , inspectionName = "Infinite: " <> nameMetaPackage <> "/" <> nameMetaName
    , inspectionDescription =
        "Usage of the '" <> nameMetaName <> "' function that hangs on infinite lists"
    , inspectionSolution = []
    , inspectionCategory = Category.infinite :| [Category.list]
    , inspectionSeverity = Warning
    , inspectionAnalysis = FindName nameMeta pat
    }

-- | 'Inspection' for 'stan0101' — infinite 'reverse' @STAN-0101@.
stan0101 :: Inspection
stan0101 = mkInfiniteInspection (Id "STAN-0101") (mkBaseListMeta "reverse") PatternAnything

-- | 'Inspection' for 'stan0102' — infinite 'Data.OldList.isSuffixOf' @STAN-0102@.
stan0102 :: Inspection
stan0102 = mkInfiniteInspection (Id "STAN-0102") (mkBaseOldListMeta "isSuffixOf") PatternAnything

-- | 'Inspection' for 'stan0103' — infinite 'Data.Foldable.length' @STAN-0103@.
stan0103 :: Inspection
stan0103 = mkInfiniteInspection (Id "STAN-0103") (mkBaseFoldableMeta "length") (PatternFun patternList PatternAnything)

-- | 'Inspection' for 'stan0104' — infinite 'Data.OldList.genericLength' @STAN-0104@.
stan0104 :: Inspection
stan0104 = mkInfiniteInspection (Id "STAN-0104") (mkBaseOldListMeta "genericLength") PatternAnything

-- | 'Inspection' for 'stan0105' — infinite 'Data.Foldable.sum' @STAN-0105@.
stan0105 :: Inspection
stan0105 = mkInfiniteInspection (Id "STAN-0105") (mkBaseFoldableMeta "sum") (PatternFun patternList PatternAnything)

-- | 'Inspection' for 'stan0106' — infinite 'Data.Foldable.product' @STAN-0106@.
stan0106 :: Inspection
stan0106 = mkInfiniteInspection (Id "STAN-0106") (mkBaseFoldableMeta "product") (PatternFun patternList PatternAnything)
