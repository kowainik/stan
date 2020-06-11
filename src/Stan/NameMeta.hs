{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Data types and functions for working with meta information.
-}

module Stan.NameMeta
    ( NameMeta (..)

      -- * Pretty show
    , prettyShowNameMeta

      -- * Comparison with 'Name'
    , compareNames
    , hieMatchNameMeta

      -- * Smart constructors
    , baseNameFrom
    , mkBaseListMeta
    , mkBaseOldListMeta
    , mkBaseFoldableMeta

    , unorderedNameFrom
    ) where

import HieTypes (ContextInfo (IEThing), IEType (Import), Identifier, IdentifierDetails (..),
                 TypeIndex)
import Module (moduleUnitId)
import Name (Name, isExternalName, nameModule, nameOccName)
import OccName (occNameString)

import Stan.Core.ModuleName (ModuleName (..), fromGhcModule)

import qualified Data.Set as Set
import qualified Data.Text as T


-- | Meta information about function/type.
data NameMeta = NameMeta
    { nameMetaPackage    :: !Text
    , nameMetaModuleName :: !ModuleName
    , nameMetaName       :: !Text
    } deriving stock (Show, Eq)

-- | Pretty show 'NameMeta' in the following format: @package\/module\/name@.
prettyShowNameMeta :: NameMeta -> Text
prettyShowNameMeta NameMeta{..} = T.intercalate "/"
    [ nameMetaPackage
    , unModuleName nameMetaModuleName
    , nameMetaName
    ]

-- | Check if 'NameMeta' is identical to 'Name'.
compareNames :: NameMeta -> Name -> Bool
compareNames NameMeta{..} name =
    let occName = toText $ occNameString $ nameOccName name
        moduleName = fromGhcModule $ nameModule name
        package = show @Text $ moduleUnitId $ nameModule name
    in
        isExternalName name
        && occName    == nameMetaName
        && moduleName == nameMetaModuleName
        && nameMetaPackage `T.isPrefixOf` package

{- | Check whether HIE 'Identifier' with details is a given 'NameMeta'.
-}
hieMatchNameMeta
    :: NameMeta  -- ^ Name meta info
    -> (Identifier, IdentifierDetails TypeIndex)  -- ^ HIE identifier
    -> Bool
hieMatchNameMeta nameMeta (identifier, details) = isJust $ do
    -- check: not a module name
    Right name <- Just identifier
    guard
        -- not in the imports
        $ Set.notMember (IEThing Import) (identInfo details)
        -- exact name/module/package
        && compareNames nameMeta name

{- | Create 'NameMeta' for a function from the @base@ package and
a given 'ModuleName'. module.
-}
infix 8 `baseNameFrom`
baseNameFrom :: Text -> ModuleName -> NameMeta
baseNameFrom funName moduleName = NameMeta
    { nameMetaName       = funName
    , nameMetaModuleName = moduleName
    , nameMetaPackage    = "base"
    }

{- | Create 'NameMeta' for a function from the @base@ package and
the "GHC.List" module.
-}
mkBaseListMeta :: Text -> NameMeta
mkBaseListMeta = (`baseNameFrom` "GHC.List")

{- | Create 'NameMeta' for a function from the @base@ package and
the "Data.OldList" module.
-}
mkBaseOldListMeta :: Text -> NameMeta
mkBaseOldListMeta = (`baseNameFrom` "Data.OldList")

{- | Create 'NameMeta' for a function from the @base@ package and
the "Data.Foldable" module.
-}
mkBaseFoldableMeta :: Text -> NameMeta
mkBaseFoldableMeta = (`baseNameFrom` "Data.Foldable")

{- | Create 'NameMeta' for a function from the @unordered-containers@ package
and a given 'ModuleName' module.
-}
infix 8 `unorderedNameFrom`
unorderedNameFrom :: Text -> ModuleName -> NameMeta
unorderedNameFrom funName moduleName = NameMeta
    { nameMetaName       = funName
    , nameMetaModuleName = moduleName
    , nameMetaPackage    = "unordered-containers"
    }
