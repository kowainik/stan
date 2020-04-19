{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Contains all 'Inspection's for partial functions.
-}

module Stan.Inspection.Partial
    ( -- * Partial 'Inspection's
      stan0001
    , stan0001Inspection
    , stan0001Meta

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

-- | 'Id' for the partial 'head' 'Inspection' — @STAN-0001@.
stan0001 :: Id Inspection
stan0001 = Id "STAN-0001"

-- | Corresponding 'Inspection' for 'stan0001' — partial 'GHC.List.head' @STAN-0001@.
stan0001Inspection :: Inspection
stan0001Inspection = (mkPartialInspection stan0001 stan0001Meta)
    { inspectionSolution =
        [ "Replace list with 'NonEmpty' from 'Data.List.NonEmpty'"
        , "Use explicit pattern-matching over lists"
        ]
    }

-- | Corresponding 'NameMeta' for 'stan0001' — partial 'GHC.List.head' @STAN-0001@.
stan0001Meta :: NameMeta
stan0001Meta = NameMeta
    { nameMetaName       = "head"
    , nameMetaPackage    = "base"
    , nameMetaModuleName = "GHC.List"
    }
