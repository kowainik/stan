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

@stan@ runtime configuration that allows customizing the set of
inspections to check the code against.
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
    , mkDefaultChecks

      -- * Final stage
    , finaliseConfig

      -- * Printing
    , configToCliCommand

      -- * Apply config
      -- $applyConfig
    , applyChecks
    ) where

import Trial ((::-), Phase (..), Trial, withTag)

import Stan.Category (Category (..))
import Stan.Core.Id (Id (..))
import Stan.Inspection (Inspection (..))
import Stan.Inspection.All (inspections, inspectionsIds, lookupInspectionById)
import Stan.Observation (Observation (..))
import Stan.Severity (Severity (..))

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Text as T


{- | Main configuration type for the following purposes:

* Filtering inspections (including or ignoring) per scope (file,
  directory, all)
-}
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

-- | Type of 'Check': 'Include' or 'Ignore' 'Inspection's.
data CheckType
    = Include
    | Ignore
    deriving stock (Show, Eq, Enum, Bounded)

-- | Rule to control the set of inspections per scope.
data Check = Check
    { checkType   :: !CheckType
    , checkFilter :: !(Maybe CheckFilter)
    , checkScope  :: !(Maybe CheckScope)
    } deriving stock (Show, Eq)

-- | Criterion for inspections filtering.
data CheckFilter
    = CheckInspection (Id Inspection)
    | CheckObservation (Id Observation)
    | CheckSeverity Severity
    | CheckCategory Category
    deriving stock (Show, Eq)

-- | Where to apply the rule for controlling inspection set.
data CheckScope
    = CheckScopeFile FilePath
    | CheckScopeDirectory FilePath
    deriving stock (Show, Eq)

defaultConfig :: PartialConfig
defaultConfig = ConfigP
    { configChecks = withTag "Default" $ pure []
    }

finaliseConfig :: PartialConfig -> Trial Text Config
finaliseConfig config = do
    configChecks <- #configChecks config
    pure ConfigP {..}


{- | Convert TOML configuration to the equivalent CLI command that can
be copy-pasted to get the same results as using the TOML config.

@
  ⓘ Reading Configurations from \/home\/vrom911\/Kowainik\/stan\/.stan.toml ...
stan check --ignore --directory=test/ \\
     check --include \\
     check --ignore --inspectionId=STAN-0002 \\
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

mkDefaultChecks :: [FilePath] -> HashMap FilePath (HashSet (Id Inspection))
mkDefaultChecks = HashMap.fromList . map (, inspectionsIds)

{- | Convert the list of 'Check's from 'Config' to data structure that
allows filtering of 'Inspection's.
-}
applyChecks :: [FilePath] -> [Check] -> HashMap FilePath (HashSet (Id Inspection))
applyChecks paths = foldl' useCheck (mkDefaultChecks paths)
  where
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
            CheckScopeFile path -> HashMap.adjust f path hm
            CheckScopeDirectory dir -> HashMap.mapWithKey
                (\path -> if isInDir dir path then f else id)
                hm

    isInDir :: FilePath -> FilePath -> Bool
    isInDir dir path = dir `isPrefixOf` path

{- $applyConfig

The 'applyConfig' function transforms the list of rules defined in the
'Config' (either via TOML or CLI) to get the list of 'Inspection's for
each module.

By default, @stan@ runs all 'Inspection's for all modules in the
Haskell project and outputs all 'Observation's it finds. Using
'Config', you can adjust the default setting using your preferences.

=== Algorithm

The algorithm for figuring out the resulting set of 'Inspection's per
module applies each 'Check' one-by-one in order of their appearance.

When introducing a new 'Check' in the config, you must always specify
the 'CheckType' (either 'Include' or 'Ignore'). 'CheckFilter' and
'CheckScope' can be omitted. If they are not written explicitly, then
the 'Check' is applied to all the entries. Specifically:

* If 'CheckFilter' is not specified, 'Check' applies to all
  inspections for the given 'CheckScope' (either 'Ignore' or 'Include'
  all 'Inspection's)
* If 'CheckScope' is not specified, 'Check' applies given
  'CheckFilter' to all files

As a result of this approach, when both 'CheckFilter' and 'CheckScope'
are not specified explicitly, either all 'Inspection's are 'Ignore'd,
or all inspections are 'Include'd back.

The algorithm doesn't remove any files or inspections from the
consideration completely. So, for example, if you ignore all
inspections in a specific file, new inspections can be added for this
file later by the follow up rules. If you want to ignore some files or
directories completely, put 'Check's for them at the end of your
configuration.

=== Common examples

This section contains examples of custom configuration (in TOML) for
common cases.

1. Ignore all 'Inspection's.

    @
    [[check]]
    type = \"Ignore\"
    @

2. Ignore all 'Inspection's only for specific file.

    @
    [[check]]
    type = \"Ignore\"
    file = "src/MyModule.hs"
    @

3. Ignore a specific 'Inspection' in all files:

    @
    [[check]]
    type = \"Ignore\"
    inspectionId = "STAN-0001"
    @

4. Ignore all 'Inspection's for specific file except 'Inspection's
that have a category @Partial@.

    @
    # ignore all inspections for a file
    [[check]]
    type = \"Ignore\"
    file = "src/MyModule.hs"

    # return back only required inspections
    [[check]]
    type = \"Include\"
    category = \"Partial\"
    file = "src/MyModule.hs"
    @

5. Keep 'Inspection's only with the category @Partial@ for all files
except a single one.

    @
    # ignore all inspections
    [[check]]
    type = \"Ignore\"

    # return back inspections with the category Partial
    [[check]]
    type = \"Include\"
    category = \"Partial\"

    # finally, disable all inspections for a specific file
    [[check]]
    type = \"Ignore\"
    file = "src/MyModule.hs"
    @
-}
