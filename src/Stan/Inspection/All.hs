{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Contains lists of all 'Inspection's and 'Inspection' 'Id's provided by @Stan@.
-}

module Stan.Inspection.All
    ( inspectionsMap
    , inspections
    , inspectionsIds

      -- * Stan inspections search
    , lookupInspectionById
    , getInspectionById
    ) where

import Stan.Core.Id (Id (..))
import Stan.Inspection (Inspection (..), InspectionsMap)
import Stan.Inspection.Infinite (infiniteInspectionsMap)
import Stan.Inspection.Partial (partialInspectionsMap)

import qualified Data.HashMap.Strict as HM


-- | All 'Inspection's map from 'Id's.
inspectionsMap :: InspectionsMap
inspectionsMap =
    partialInspectionsMap
    <> infiniteInspectionsMap

{- | List of all inspections.
-}
inspections :: [Inspection]
inspections = HM.elems inspectionsMap

-- | List of all inspection 'Id's.
inspectionsIds :: [Id Inspection]
inspectionsIds = HM.keys inspectionsMap

-- | Look up 'Inspection' by the given inspection 'Id'.
lookupInspectionById :: Id Inspection -> Maybe Inspection
lookupInspectionById insId = HM.lookup insId inspectionsMap
{-# INLINE lookupInspectionById #-}

-- | Get the 'Inspection' by the given known inspection 'Id'.
getInspectionById :: Id Inspection -> Inspection
getInspectionById insId = case lookupInspectionById insId of
    Just ins -> ins
    Nothing  -> error $ "Unknown Inspection ID: " <> unId insId
