{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Static analysis summary.
-}

module Stan.Analysis.Summary
    ( Summary (..)
    , createSummary
    ) where

import Stan.Analysis (Analysis (..))
import Stan.Category (Category)
import Stan.Core.Id (Id)
import Stan.Core.ModuleName (ModuleName)
import Stan.Inspection (Inspection (..))
import Stan.Inspection.All (getInspectionById)
import Stan.Observation (Observation (..))
import Stan.Severity (Severity)

import qualified Data.HashMap.Strict as HM


-- | Short info about analysis.
data Summary = Summary
    { summaryInspectionId :: !(Id Inspection)  -- ^ The most popular 'Inspection'
    , summaryCategory     :: !Category  -- ^ The most popular 'Category'
    , summaryModule       :: !ModuleName  -- ^ Module with the biggest number of observations
    , summarySeverity     :: !Severity  -- ^ The highest 'Severity'
    }

data SummaryData = SummaryData
    { sdInspections :: !(HashMap (Id Inspection) Int)  -- ^ Count of each inspection
    , sdCategories  :: !(HashMap Category Int)  -- ^ Count of each category
    , sdModules     :: !(HashMap ModuleName Int)  -- ^ Count of observations per module
    , sdSeverity    :: !(Maybe Severity)  -- ^ Highest severity
    }

emptySummaryData :: SummaryData
emptySummaryData = SummaryData
    { sdInspections = mempty
    , sdCategories  = mempty
    , sdModules     = mempty
    , sdSeverity    = Nothing
    }

summaryFromData :: SummaryData -> Maybe Summary
summaryFromData SummaryData{..} = do
    summaryInspectionId <- maxOnSnd $ HM.toList sdInspections
    summaryCategory     <- maxOnSnd $ HM.toList sdCategories
    summaryModule       <- maxOnSnd $ HM.toList sdModules
    summarySeverity     <- sdSeverity
    pure Summary{..}
  where
    maxOnSnd :: Ord b => [(a, b)] -> Maybe a
    maxOnSnd = viaNonEmpty (fst . maximumOn1 snd)

{- | Assemble 'Summary' after analysis. Returns 'Nothing' when there's
no 'Observations'. Otherwise, there's at least one observation, which
means that we can find the most popular 'Inspection', 'Category' and
the highest 'Severity'.
-}
createSummary :: Analysis -> Maybe Summary
createSummary Analysis{..} = summaryFromData
    $ foldl' handleObservation emptySummaryData analysisObservations
  where
    handleObservation :: SummaryData -> Observation -> SummaryData
    handleObservation SummaryData{..} Observation{..} =
        let Inspection{..} = getInspectionById observationInspectionId
        in SummaryData
            -- increase count for the current observations
            { sdInspections = HM.insertWith (+) observationInspectionId 1 sdInspections

            -- increase count for each category
            , sdCategories = flipfoldl'
                (\cat -> HM.insertWith (+) cat 1)
                sdCategories
                inspectionCategory

            -- increase count the module
            , sdModules = HM.insertWith (+) observationModuleName 1 sdModules

            -- choose the highest severity
            , sdSeverity = max sdSeverity (Just inspectionSeverity)
            }

-- | Fast, strict maximum
maximumOn1 :: forall b a . Ord b => (a -> b) -> NonEmpty a -> a
maximumOn1 f (x :| xs) = foldl' cmp x xs
  where
    cmp :: a -> a -> a
    cmp a b = case compare (f a) (f b) of
        LT -> b
        EQ -> a
        GT -> a
