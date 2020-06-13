{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Contains all 'Inspection's for functions that hang on infinite lists.

The __infinite__ inspections are in ranges:

* @STAN-0101 .. STAN-0200@
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

import Relude.Extra.Tuple (fmapToFst)

import Stan.Core.Id (Id (..))
import Stan.Inspection (Inspection (..), InspectionAnalysis (..), InspectionsMap)
import Stan.NameMeta (NameMeta (..), mkBaseFoldableMeta, mkBaseListMeta, mkBaseOldListMeta)
import Stan.Pattern.Ast (namesToPatternAst)
import Stan.Pattern.Edsl (PatternBool (..))
import Stan.Pattern.Type (PatternType (..), listFunPattern)
import Stan.Severity (Severity (..))

import qualified Stan.Category as Category


-- | All infinite 'Inspection's map from 'Id's.
infiniteInspectionsMap :: InspectionsMap
infiniteInspectionsMap = fromList $ fmapToFst inspectionId
    [ stan0101
    , stan0102
    , stan0103
    , stan0104
    , stan0105
    , stan0106
    ]

-- | Smart constructor to create infinite 'Inspection'.
mkInfiniteInspection :: Id Inspection -> NonEmpty (NameMeta, PatternType) -> Inspection
mkInfiniteInspection insId pats@((NameMeta{..},_) :| _) = Inspection
    { inspectionId = insId
    , inspectionName = "Infinite: " <> nameMetaPackage <> "/" <> nameMetaName
    , inspectionDescription =
        "Usage of the '" <> nameMetaName <> "' function that hangs on infinite lists"
    , inspectionSolution =
        [ "Don't use '" <> nameMetaName <> "' if you expect your function to work with infinite lists"
        , "Use 'slist' package for fast and safe functions on infinite lists"
        ]
    , inspectionCategory = Category.infinite :| [Category.list]
    , inspectionSeverity = PotentialBug
    , inspectionAnalysis = FindAst $ namesToPatternAst pats
    }

-- | 'Inspection' for 'stan0101' — infinite 'reverse' @STAN-0101@.
stan0101 :: Inspection
stan0101 = mkInfiniteInspection (Id "STAN-0101") $ one (mkBaseListMeta "reverse", (?))

-- | 'Inspection' for 'stan0102' — infinite 'Data.OldList.isSuffixOf' @STAN-0102@.
stan0102 :: Inspection
stan0102 = mkInfiniteInspection (Id "STAN-0102") $ one (mkBaseOldListMeta "isSuffixOf", (?))

-- | 'Inspection' for 'stan0103' — infinite 'Data.Foldable.length' | 'Data.OldList.length' @STAN-0103@.
stan0103 :: Inspection
stan0103 = mkInfiniteInspection (Id "STAN-0103") $
        (mkBaseFoldableMeta "length", listFunPattern)
    :| [(mkBaseListMeta     "length", listFunPattern)]

-- | 'Inspection' for 'stan0104' — infinite 'Data.OldList.genericLength' @STAN-0104@.
stan0104 :: Inspection
stan0104 = mkInfiniteInspection (Id "STAN-0104") $ one (mkBaseOldListMeta "genericLength", (?))

-- | 'Inspection' for 'stan0105' — infinite 'Data.Foldable.sum' @STAN-0105@.
stan0105 :: Inspection
stan0105 = mkInfiniteInspection (Id "STAN-0105") $ one (mkBaseFoldableMeta "sum", listFunPattern)

-- | 'Inspection' for 'stan0106' — infinite 'Data.Foldable.product' @STAN-0106@.
stan0106 :: Inspection
stan0106 = mkInfiniteInspection (Id "STAN-0106") $ one (mkBaseFoldableMeta "product", listFunPattern)
