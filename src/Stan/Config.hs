{-# LANGUAGE ApplicativeDo        #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

@stan@ configurations.
-}

module Stan.Config
    ( -- * Data types
      ConfigP (..)
    , Config
    , PartialConfig
    , Check (..)
    , CheckType (..)
    , CheckFilter (..)
    , CheckScope (..)

      -- * Default
    , defaultConfig

      -- * Final stage
    , finaliseConfig

      -- * Printing
    , configToCliCommand

      -- * Apply config
    , applyChecks
    ) where

import Trial ((::-), Phase (..), Trial, withTag)

import Stan.Category (Category (..))
import Stan.Core.Id (Id (..))
import Stan.Core.ModuleName (ModuleName (..))
import Stan.Inspection (Inspection (..))
import Stan.Inspection.All (inspections, inspectionsIds, lookupInspectionById)
import Stan.Observation (Observation (..))
import Stan.Severity (Severity (..))

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Text as T


data ConfigP (p :: Phase Text) = ConfigP
    { configChecks :: !(p ::- [Check])
    -- , configGroupBy :: !GroupBy
    }

deriving stock instance
    ( Show (p ::- [Check])
    ) => Show (ConfigP p)

deriving stock instance
    ( Eq (p ::- [Check])
    ) => Eq (ConfigP p)

type Config = ConfigP 'Final
type PartialConfig = ConfigP 'Partial

instance Semigroup PartialConfig where
    (<>) :: PartialConfig -> PartialConfig -> PartialConfig
    x <> y = ConfigP
        { configChecks = configChecks x <> configChecks y
        }

data CheckType
    = Include
    | Ignore
    deriving stock (Show, Eq, Enum, Bounded)

data Check = Check
    { checkType   :: !CheckType
    , checkFilter :: !(Maybe CheckFilter)
    , checkScope  :: !(Maybe CheckScope)
    } deriving stock (Show, Eq)

data CheckFilter
    = CheckInspection (Id Inspection)
    | CheckObservation (Id Observation)
    | CheckSeverity Severity
    | CheckCategory Category
    deriving stock (Show, Eq)

data CheckScope
    = CheckScopeFile FilePath
    | CheckScopeDirectory FilePath
    | CheckScopeModule ModuleName
    deriving stock (Show, Eq)

defaultConfig :: PartialConfig
defaultConfig = ConfigP
    { configChecks = withTag "Default" $ pure []
    }

finaliseConfig :: PartialConfig -> Trial Text Config
finaliseConfig config = do
    configChecks <- #configChecks config
    pure ConfigP {..}


{- |

@
  ⓘ Reading Configurations from /home/vrom911/Kowainik/stan/.stan.toml ...
stan check --ignore --directory=test/ \
     check --include \
     check --ignore --inspectionId=STAN-0002 \
     check --ignore --inspectionId=STAN-0001 --file=src/MyFile.hs
@
-}
configToCliCommand :: Config -> Text
configToCliCommand ConfigP{..} = "stan " <> T.intercalate " \\\n     " (map checkToCli configChecks)
  where
    checkToCli :: Check -> Text
    checkToCli Check{..} = "check"
        <> checkTypeToCli checkType
        <> maybe "" checkFilterToCli checkFilter
        <> maybe "" checkScopeToCli checkScope

    checkTypeToCli :: CheckType -> Text
    checkTypeToCli = \case
        Include -> " --include"
        Ignore  -> " --ignore"

    checkFilterToCli :: CheckFilter -> Text
    checkFilterToCli = \case
        CheckInspection insId -> " --inspectionId=" <> unId insId
        CheckObservation obsId -> " --observationId=" <> unId obsId
        CheckSeverity sev -> " --severity=" <> show sev
        CheckCategory cat -> " --category=" <> unCategory cat

    checkScopeToCli :: CheckScope -> Text
    checkScopeToCli = \case
        CheckScopeFile file -> " --file=" <> toText file
        CheckScopeDirectory dir -> " --directory=" <> toText dir
        CheckScopeModule m -> " --module=" <> unModuleName m

{- | Convert the list of 'Check's from 'Config' to data structure that
allows filtering of 'Inspection's.
-}
applyChecks :: [FilePath] -> [Check] -> HashMap FilePath (HashSet (Id Inspection))
applyChecks paths = foldl' useCheck filesMap
  where
    filesMap :: HashMap FilePath (HashSet (Id Inspection))
    filesMap = HashMap.fromList $ map (, inspectionsIds) paths

    useCheck
        :: HashMap FilePath (HashSet (Id Inspection))
        -> Check
        -> HashMap FilePath (HashSet (Id Inspection))
    useCheck dict Check{..} =
        applyForScope (applyFilter checkType checkFilter) checkScope dict

    applyFilter
        :: CheckType
        -> Maybe CheckFilter
        -> HashSet (Id Inspection)
        -> HashSet (Id Inspection)
    applyFilter = \case
        Include -> includeFilter
        Ignore -> ignoreFilter

    ignoreFilter :: Maybe CheckFilter -> HashSet (Id Inspection) -> HashSet (Id Inspection)
    ignoreFilter cFilter = HashSet.filter (not . satisfiesFilter cFilter)

    includeFilter :: Maybe CheckFilter -> HashSet (Id Inspection) -> HashSet (Id Inspection)
    includeFilter mFilter ins = case mFilter of
         -- add all inspections to the existing ones
         Nothing -> inspectionsIds <> ins
         Just cFilter -> case cFilter of
             CheckInspection iId -> HashSet.insert iId ins
             CheckObservation _ -> ins
             CheckSeverity sev ->
                 let sevInspections = filter ((== sev) . inspectionSeverity) inspections
                 in HashSet.fromList (map inspectionId sevInspections) <> ins
             CheckCategory cat ->
                 let catInspections = filter (elem cat . inspectionCategory) inspections
                 in HashSet.fromList (map inspectionId catInspections) <> ins

    -- Returns 'True' if the given inspection satisfies 'CheckFilter'
    satisfiesFilter :: Maybe CheckFilter -> Id Inspection -> Bool
    satisfiesFilter mFilter iId = case mFilter of
        Nothing -> True  -- no filter, always satisfies
        -- TODO: rewrite more efficiently after using GHC-8.10
        Just cFilter -> case lookupInspectionById iId of
            Nothing -> False  -- no such ID => doesn't satisfy
            Just Inspection{..} -> case cFilter of
                CheckInspection checkId -> iId == checkId
                CheckObservation _      -> False
                CheckSeverity sev       -> sev == inspectionSeverity
                CheckCategory cat       -> cat `elem` inspectionCategory

    applyForScope
        :: (HashSet (Id Inspection) -> HashSet (Id Inspection))
        -> Maybe CheckScope
        -> HashMap FilePath (HashSet (Id Inspection))
        -> HashMap FilePath (HashSet (Id Inspection))
    applyForScope f mScope hm = case mScope of
        Nothing -> f <$> hm  -- no scope = apply for everything
        Just cScope -> case cScope of
            CheckScopeFile path -> HashMap.alter (fmap f) path hm
            CheckScopeDirectory dir -> HashMap.mapWithKey
                (\path -> if isInDir dir path then f else id)
                hm
            CheckScopeModule _moduleName -> hm  -- TODO: no info about modules here :(

    isInDir :: FilePath -> FilePath -> Bool
    isInDir dir path = dir `isPrefixOf` path
