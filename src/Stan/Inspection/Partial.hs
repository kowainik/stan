{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Contains all 'Inspection's for partial functions.
-}

module Stan.Inspection.Partial
    ( -- * Partial 'Inspection's
      -- *** Partial 'GHC.List.head'
      stan0001
    , stan0001Inspection
    , stan0001Meta
      -- *** Partial 'GHC.List.tail'
    , stan0002
    , stan0002Inspection
    , stan0002Meta
      -- *** Partial 'GHC.List.init'
    , stan0003
    , stan0003Inspection
    , stan0003Meta
      -- *** Partial 'GHC.List.last'
    , stan0004
    , stan0004Inspection
    , stan0004Meta

      -- * List of all partial 'Inspection's
    , partialInspections
    , partialInspectionsIds
    ) where

import Stan.Category (partial)
import Stan.Core.Id (Id (..))
import Stan.Inspection (Inspection (..), NameMeta (..), Severity (..))


-- | All partial 'Inspection's.
partialInspections :: [Inspection]
partialInspections =
    [ stan0001Inspection
    , stan0002Inspection
    , stan0003Inspection
    , stan0004Inspection
    ]

-- | All partial 'Inspection's 'Id's.
partialInspectionsIds :: [Id Inspection]
partialInspectionsIds = map inspectionId partialInspections

-- | Smart constructor to create partial 'Inspection'.
mkPartialInspection :: Id Inspection -> NameMeta -> Inspection
mkPartialInspection insId NameMeta{..} = Inspection
    { inspectionId = insId
    , inspectionName = "Partial: " <> nameMetaPackage <> "/" <> nameMetaName
    , inspectionDescription = "Usage of partial function '" <> nameMetaName <> "' for lists"
    , inspectionSolution = []
    , inspectionCategory = one partial
    , inspectionSeverity = Severe
    }

{- | Smart constructor to create partial 'Inspection' for functions
that work with lists.
-}
mkPartialInspectionList :: Id Inspection -> NameMeta -> Inspection
mkPartialInspectionList insId nameMeta = (mkPartialInspection insId nameMeta)
    { inspectionSolution =
        [ "Replace list with 'NonEmpty' from 'Data.List.NonEmpty'"
        , "Use explicit pattern-matching over lists"
        ]
    }

mkBaseListMeta :: Text -> NameMeta
mkBaseListMeta funName = NameMeta
    { nameMetaName       = funName
    , nameMetaPackage    = "base"
    , nameMetaModuleName = "GHC.List"
    }
-- | 'Id' for the partial 'GHC.List.head' 'Inspection' — @STAN-0001@.
stan0001 :: Id Inspection
stan0001 = Id "STAN-0001"

-- | Corresponding 'Inspection' for 'stan0001' — partial 'GHC.List.head' @STAN-0001@.
stan0001Inspection :: Inspection
stan0001Inspection = mkPartialInspectionList stan0001 stan0001Meta

-- | Corresponding 'NameMeta' for 'stan0001' — partial 'GHC.List.head' @STAN-0001@.
stan0001Meta :: NameMeta
stan0001Meta = mkBaseListMeta "head"

-- | 'Id' for the partial 'GHC.List.tail' 'Inspection' — @STAN-0002@.
stan0002 :: Id Inspection
stan0002 = Id "STAN-0002"

-- | Corresponding 'Inspection' for 'stan0002' — partial 'GHC.List.tail' @STAN-0002@.
stan0002Inspection :: Inspection
stan0002Inspection = mkPartialInspectionList stan0002 stan0002Meta

-- | Corresponding 'NameMeta' for 'stan0002' — partial 'GHC.List.tail' @STAN-0002@.
stan0002Meta :: NameMeta
stan0002Meta = mkBaseListMeta "tail"

-- | 'Id' for the partial 'GHC.List.init' 'Inspection' — @STAN-0003@.
stan0003 :: Id Inspection
stan0003 = Id "STAN-0003"

-- | Corresponding 'Inspection' for 'stan0003' — partial 'GHC.List.init' @STAN-0003@.
stan0003Inspection :: Inspection
stan0003Inspection = mkPartialInspectionList stan0003 stan0003Meta

-- | Corresponding 'NameMeta' for 'stan0003' — partial 'GHC.List.init' @STAN-0003@.
stan0003Meta :: NameMeta
stan0003Meta = mkBaseListMeta "init"

-- | 'Id' for the partial 'GHC.List.last' 'Inspection' — @STAN-0004@.
stan0004 :: Id Inspection
stan0004 = Id "STAN-0004"

-- | Corresponding 'Inspection' for 'stan0004' — partial 'GHC.List.last' @STAN-0004@.
stan0004Inspection :: Inspection
stan0004Inspection = mkPartialInspectionList stan0004 stan0004Meta

-- | Corresponding 'NameMeta' for 'stan0004' — partial 'GHC.List.last' @STAN-0004@.
stan0004Meta :: NameMeta
stan0004Meta = mkBaseListMeta "last"
