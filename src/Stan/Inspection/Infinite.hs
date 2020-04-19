{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Contains all 'Inspection's for functions that hang on infinite lists.
-}

module Stan.Inspection.Infinite
    ( -- * Infinite inspections
      stan0101
    , stan0101Inspection
    , stan0101Meta

      -- * All inspections
    , infiniteInspections
    , infiniteInspectionsIds
    ) where

import Stan.Category (infinite)
import Stan.Core.Id (Id (..))
import Stan.Inspection (Inspection (..), NameMeta (..), Severity (..))


-- | All infinite 'Inspection's.
infiniteInspections :: [Inspection]
infiniteInspections =
    [ stan0101Inspection
    ]

-- | All infinite 'Inspection's 'Id's.
infiniteInspectionsIds :: [Id Inspection]
infiniteInspectionsIds = map inspectionId infiniteInspections

-- | Smart constructor to create infinite 'Inspection'.
mkInfiniteInspection :: Id Inspection -> NameMeta -> Inspection
mkInfiniteInspection insId NameMeta{..} = Inspection
    { inspectionId = insId
    , inspectionName = "Infinite: " <> nameMetaPackage <> "/" <> nameMetaName
    , inspectionDescription =
        "Usage of the '" <> nameMetaName <> "' function that hangs on infinite lists"
    , inspectionSolution = []
    , inspectionCategory = one infinite
    , inspectionSeverity = Severe
    }

-- | 'Id' for the infinite 'reverse' 'Inspection' — @STAN-0101@.
stan0101 :: Id Inspection
stan0101 = Id "STAN-0101"

-- | Corresponding 'Inspection' for 'stan0101' — infinite 'reverse' @STAN-0101@.
stan0101Inspection :: Inspection
stan0101Inspection = mkInfiniteInspection stan0101 stan0101Meta

-- | Corresponding 'NameMeta' for 'stan0001' — infinite 'reverse' @STAN-0101@.
stan0101Meta :: NameMeta
stan0101Meta = NameMeta
    { nameMetaName       = "reverse"
    , nameMetaModuleName = "GHC.List"
    , nameMetaPackage    = "base"
    }
