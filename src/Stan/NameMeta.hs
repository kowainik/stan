{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Data types and functions for working with meta information.
-}

module Stan.NameMeta
    ( NameMeta (..)

      -- * Smart constructors
    , mkBaseListMeta
    , mkBaseOldListMeta
    ) where

import Stan.Core.ModuleName (ModuleName)


-- | Meta information about function/type.
data NameMeta = NameMeta
    { nameMetaPackage    :: !Text
    , nameMetaModuleName :: !ModuleName
    , nameMetaName       :: !Text
    } deriving stock (Show, Eq)


mkBaseListMeta :: Text -> NameMeta
mkBaseListMeta funName = NameMeta
    { nameMetaName       = funName
    , nameMetaModuleName = "GHC.List"
    , nameMetaPackage    = "base"
    }

mkBaseOldListMeta :: Text -> NameMeta
mkBaseOldListMeta funName = (mkBaseListMeta funName)
    { nameMetaModuleName = "Data.OldList"
    }
