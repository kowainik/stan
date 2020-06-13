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

import qualified Stan.Ghc.Compat as Ghc


-- | Wrapper around Haskell module name.
newtype ModuleName = ModuleName
    { unModuleName :: Text
    } deriving stock (Show)
      deriving newtype (Eq, Hashable, IsString)

-- | Convert 'GHC.ModuleName' to 'ModuleName'.
fromGhcModuleName :: Ghc.ModuleName -> ModuleName
fromGhcModuleName = ModuleName . toText . Ghc.moduleNameString

-- | Extract 'ModuleName' from 'GHC.Module'.
fromGhcModule :: Ghc.Module -> ModuleName
fromGhcModule = fromGhcModuleName . Ghc.moduleName
