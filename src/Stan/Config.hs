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
    , Scope (..)

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
    , applyChecksFor
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
    { configChecks  :: !(p ::- [Check])
    , configRemoved :: !(p ::- [Scope])
    -- , configGroupBy :: !GroupBy
    }

deriving stock instance
    ( Show (p ::- [Check])
    , Show (p ::- [Scope])
    ) => Show (ConfigP p)

deriving stock instance
    ( Eq (p ::- [Check])
    , Eq (p ::- [Scope])
    ) => Eq (ConfigP p)

type Config = ConfigP 'Final
type PartialConfig = ConfigP 'Partial

instance Semigroup PartialConfig where
    (<>) :: PartialConfig -> PartialConfig -> PartialConfig
    x <> y = ConfigP
        { configChecks = configChecks x <> configChecks y
        , configRemoved = configRemoved x <> configRemoved y
        }

-- | Type of 'Check': 'Include' or 'Ignore' 'Inspection's.
data CheckType
    = Include
    | Ignore
    deriving stock (Show, Eq, Enum, Bounded)

-- | Rule to control the set of inspections per scope.
data Check = Check
    { checkType   :: !CheckType
    , checkFilter :: !CheckFilter
    , checkScope  :: !Scope
    } deriving stock (Show, Eq)

-- | Criterion for inspections filtering.
data CheckFilter
    = CheckInspection (Id Inspection)
    | CheckObservation (Id Observation)
    | CheckSeverity Severity
    | CheckCategory Category
    | CheckAll
    deriving stock (Show, Eq)

-- | Where to apply the rule for controlling inspection set.
data Scope
    = ScopeFile FilePath
    | ScopeDirectory FilePath
    | ScopeAll
    deriving stock (Show, Eq)

defaultConfig :: PartialConfig
defaultConfig = ConfigP
    { configChecks  = withTag "Default" $ pure []
    , configRemoved = withTag "Default" $ pure []
    }

finaliseConfig :: PartialConfig -> Trial Text Config
finaliseConfig config = do
    configChecks  <- #configChecks config
    configRemoved <- #configRemoved config
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
configToCliCommand ConfigP{..} = "stan " <> T.intercalate " \\\n     "
    (  map checkToCli configChecks
    <> map removedToCli configRemoved
    )
  where
    checkToCli :: Check -> Text
    checkToCli Check{..} = "check"
        <> checkTypeToCli checkType
        <> checkFilterToCli checkFilter
        <> scopeToCli checkScope

    removedToCli :: Scope -> Text
    removedToCli scope = "remove"
        <> scopeToCli scope

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
        CheckAll -> " --filter-all"

    scopeToCli :: Scope -> Text
    scopeToCli = \case
        ScopeFile file -> " --file=" <> toText file
        ScopeDirectory dir -> " --directory=" <> toText dir
        ScopeAll -> " --scope-all"

mkDefaultChecks :: [FilePath] -> HashMap FilePath (HashSet (Id Inspection))
mkDefaultChecks = HashMap.fromList . map (, inspectionsIds)

{- | Convert the list of 'Check's from 'Config' to data structure that
allows filtering of 'Inspection's for given files.
-}
applyChecks
    :: [FilePath]
    -- ^ Paths to project files
    -> [Check]
    -- ^ List of rules
    -> HashMap FilePath (HashSet (Id Inspection))
    -- ^ Resulting set of inspections for each file
applyChecks = applyChecksFor . mkDefaultChecks

{- | Modify existing 'Check's for each file using the given list of
'Check's.
-}
applyChecksFor
    :: HashMap FilePath (HashSet (Id Inspection))
    -- ^ Initial set of inspections for each file
    -> [Check]
    -- ^ List of rules
    -> HashMap FilePath (HashSet (Id Inspection))
    -- ^ Resulting set of inspections for each file
applyChecksFor = foldl' useCheck
  where
    useCheck
        :: HashMap FilePath (HashSet (Id Inspection))
        -> Check
        -> HashMap FilePath (HashSet (Id Inspection))
    useCheck dict Check{..} =
        applyForScope (applyFilter checkType checkFilter) checkScope dict

    applyFilter
        :: CheckType
        -> CheckFilter
        -> HashSet (Id Inspection)
        -> HashSet (Id Inspection)
    applyFilter = \case
        Include -> includeFilter
        Ignore -> ignoreFilter

    ignoreFilter :: CheckFilter -> HashSet (Id Inspection) -> HashSet (Id Inspection)
    ignoreFilter cFilter = HashSet.filter (not . satisfiesFilter cFilter)

    includeFilter :: CheckFilter -> HashSet (Id Inspection) -> HashSet (Id Inspection)
    includeFilter cFilter ins = case cFilter of
        CheckInspection iId -> HashSet.insert iId ins
        CheckObservation _ -> ins
        CheckSeverity sev ->
            let sevInspections = filter ((== sev) . inspectionSeverity) inspections
            in HashSet.fromList (map inspectionId sevInspections) <> ins
        CheckCategory cat ->
            let catInspections = filter (elem cat . inspectionCategory) inspections
            in HashSet.fromList (map inspectionId catInspections) <> ins
        CheckAll -> inspectionsIds <> ins

    -- Returns 'True' if the given inspection satisfies 'CheckFilter'
    satisfiesFilter :: CheckFilter -> Id Inspection -> Bool
    satisfiesFilter cFilter iId = case lookupInspectionById iId of
        -- TODO: rewrite more efficiently after using GHC-8.10
        Nothing -> False  -- no such ID => doesn't satisfy
        Just Inspection{..} -> case cFilter of
            CheckInspection checkId -> iId == checkId
            CheckObservation _      -> False
            CheckSeverity sev       -> sev == inspectionSeverity
            CheckCategory cat       -> cat `elem` inspectionCategory
            CheckAll                -> True

    applyForScope
        :: (HashSet (Id Inspection) -> HashSet (Id Inspection))
        -> Scope
        -> HashMap FilePath (HashSet (Id Inspection))
        -> HashMap FilePath (HashSet (Id Inspection))
    applyForScope f cScope hm = case cScope of
        ScopeFile path -> HashMap.adjust f path hm
        ScopeDirectory dir -> HashMap.mapWithKey
            (\path -> if isInDir dir path then f else id)
            hm
        ScopeAll -> f <$> hm

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
three key-value pairs:

1. 'CheckType' — control inclusion and exclusion criteria

    * 'Include'

        @
        type = \"Include\"
        @

    * 'Ignore'

        @
        type = \"Ignore\"
        @

2. 'CheckFilter' — how to filter inspections

    * 'CheckInspection': by specific 'Inspection' 'Id'

        @
        inspectionId = "STAN-0001"
        @

    * 'CheckSeverity': by specific 'Severity'

        @
        severity = \"Warning\"
        @

    * 'CheckCategory': by specific 'Category'

        @
        category = \"Partial\"
        @

    * 'CheckAll': applied to all 'Inspection's

        @
        filter = "all"
        @

3. 'Scope' — where to apply check

    * 'ScopeFile': only to the specific file

        @
        file = "src\/MyModule.hs"
        @

    * 'ScopeDirectory': to all files in the specified directory

        @
        directory = "text\/"
        @

    * 'ScopeAll': to all files

        @
        scope = "all"
        @

The algorithm doesn't remove any files or inspections from the
consideration completely. So, for example, if you ignore all
inspections in a specific file, new inspections can be added for this
file later by the follow up rules.

However, if you want to completely remove some files or directory from
analysis, you can use the @remove@ key:

@
[[remove]]
file = "src\/Autogenerated.hs"
@

=== Common examples

This section contains examples of custom configuration (in TOML) for
common cases.

1. Ignore all 'Inspection's.

    @
    [[check]]
    type   = \"Ignore\"
    filter = "all"
    scope  = "all"
    @

2. Ignore all 'Inspection's only for specific file.

    @
    [[check]]
    type = \"Ignore\"
    filter = "all"
    file = "src/MyModule.hs"
    @

3. Ignore a specific 'Inspection' in all files:

    @
    [[check]]
    type = \"Ignore\"
    inspectionId = "STAN-0001"
    scope = "all"
    @

4. Ignore all 'Inspection's for specific file except 'Inspection's
that have a category @Partial@.

    @
    # ignore all inspections for a file
    [[check]]
    type = \"Ignore\"
    filter = "all"
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
    type   = \"Ignore\"
    filter = "all"
    scope  = "all"

    # return back inspections with the category Partial
    [[check]]
    type = \"Include\"
    category = \"Partial\"
    scope = "all"

    # finally, disable all inspections for a specific file
    [[check]]
    type = \"Ignore\"
    filter = "all"
    file = "src/MyModule.hs"
    @
-}
