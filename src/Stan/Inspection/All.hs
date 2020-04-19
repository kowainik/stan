{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Contains lists of all 'Inspection's and 'Inspection' 'Id's provided by @Stan@.
-}

module Stan.Inspection.All
    ( inspections
    , inspectionsIds

      -- * Stan inspections search
    , getInspectionById

    ) where

import Stan.Core.Id (Id (..))
import Stan.Inspection (Inspection (..))
import Stan.Inspection.Partial (partialInspections, partialInspectionsIds)


{- | List of all inspections.
-}
inspections :: [Inspection]
inspections =
    partialInspections

inspectionsIds :: [Id Inspection]
inspectionsIds =
    partialInspectionsIds

-- | Get the 'Inspection' by the given known inspection 'Id'.
getInspectionById :: Id Inspection -> Inspection
getInspectionById insId = case find ((==) insId . inspectionId) inspections of
    Just ins -> ins
    -- TODO: how to handle unknown ids better?
    Nothing  -> error $ "Unknown Inspection ID: " <> unId insId
