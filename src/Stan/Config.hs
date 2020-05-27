{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

@stan@ configurations.
-}

module Stan.Config
    ( -- * Data types
      Config (..)
    , Check (..)
    , CheckType (..)
    , CheckFilter (..)
    , CheckScope (..)
    ) where

import Stan.Category (Category (..))
import Stan.Core.Id (Id (..))
import Stan.Core.ModuleName (ModuleName (..))
import Stan.Inspection (Inspection (..))
import Stan.Observation (Observation (..))
import Stan.Severity (Severity (..))


data Config = Config
    { configChecks :: ![Check]
    -- , configGroupBy :: !GroupBy
    } deriving stock (Show, Eq)

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
