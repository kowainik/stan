{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Wrapper around Haskell module names and conversion functions for GHC
types.
-}

module Stan.Core.ModuleName
    ( ModuleName (..)
    , fromGhcModule
    , fromGhcModuleName
    ) where

import qualified Module as GHC


-- | Wrapper around Haskell module name.
newtype ModuleName = ModuleName
    { unModuleName :: Text
    } deriving stock (Show)
      deriving newtype (Eq, IsString)

-- | Convert 'GHC.ModuleName' to 'ModuleName'.
fromGhcModuleName :: GHC.ModuleName -> ModuleName
fromGhcModuleName = ModuleName . toText . GHC.moduleNameString

-- | Extract 'ModuleName' from 'GHC.Module'.
fromGhcModule :: GHC.Module -> ModuleName
fromGhcModule = fromGhcModuleName . GHC.moduleName
