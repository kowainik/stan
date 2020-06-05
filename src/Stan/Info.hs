{-# LANGUAGE TemplateHaskell #-}

{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

@stan@ build information.
-}

module Stan.Info
    ( -- * Version
      StanVersion (..)
    , stanVersion
    , prettyStanVersion

      -- * System
    , StanSystem (..)
    , stanSystem

      -- * Env
    , StanEnv (..)

      -- * Project Info
    , ProjectInfo (..)
    ) where

import Colourista (blue, bold, formatWith)
import Data.Version (showVersion)
import Development.GitRev (gitCommitDate, gitHash)
import System.Info (arch, compilerName, compilerVersion, os)

import qualified Paths_stan as Meta (version)


-- | @stan@ version information.
data StanVersion = StanVersion
    { svVersion     :: !String
    , svGitRevision :: !String
    , svCommitDate  :: !String
    } deriving stock (Show, Eq)

{- | Current @stan@ version information.
-}
stanVersion :: StanVersion
stanVersion = StanVersion
    { svVersion     = showVersion Meta.version
    , svGitRevision = $(gitHash)
    , svCommitDate  = $(gitCommitDate)
    }

{- | Colourful pretty 'StanVersion' representation used in the @CLI@.
-}
prettyStanVersion :: StanVersion -> String
prettyStanVersion StanVersion{..} = toString $ intercalate "\n"
    [ sVersion
    , sHash
    , sDate
    ]
  where
    fmt :: String -> String
    fmt = formatWith [blue, bold]

    sVersion, sHash, sDate :: String
    sVersion = fmt $ "Stan " <> "v" <> svVersion
    sHash = " ➤ " <> fmt "Git revision: " <> svGitRevision
    sDate = " ➤ " <> fmt "Commit date:  " <> svCommitDate

{- | Contains all @stan@ System information
-}
data StanSystem = StanSystem
    { ssOs              :: !String
    , ssArch            :: !String
    , ssCompiler        :: !String
    , ssCompilerVersion :: !String
    } deriving stock (Show, Eq)

-- | All system info for the project
stanSystem :: StanSystem
stanSystem = StanSystem
    { ssOs              = os
    , ssArch            = arch
    , ssCompiler        = compilerName
    , ssCompilerVersion = showVersion compilerVersion
    }

{- | Data from different environment resources:

* Environment variables
* Used TOML configuration files
* Command Line arguments
-}
data StanEnv = StanEnv
    { seEnvVars   :: !Text
    , seTomlFiles :: ![FilePath]
    , seCliArgs   :: ![String]
    } deriving stock (Show, Eq)


data ProjectInfo = ProjectInfo
    { piName       :: !String
    , piCabalFiles :: ![FilePath]
    , piHieDir     :: !FilePath
    , piFileNumber :: !Int
    } deriving stock (Show, Eq)
