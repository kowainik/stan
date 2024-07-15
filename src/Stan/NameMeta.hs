{-# LANGUAGE CPP #-}

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
    , hieFindIdentifier

      -- * Smart constructors
    , baseNameFrom
    , ghcInternalNameFrom
    , _nameFrom
    , mkBaseListMeta
    , mkBaseOldListMeta
    , mkBaseFoldableMeta

    , unorderedNameFrom
    , plutusTxNameFrom
    , textNameFrom

    , ghcPrimNameFrom
    , primTypeMeta
    ) where

import Stan.Core.ModuleName (ModuleName (..), fromGhcModule)
import Stan.Ghc.Compat (Name, isExternalName, moduleUnitId, nameModule, nameOccName, occNameString, showTUnitId)
import Stan.Hie.Compat (ContextInfo (IEThing), HieAST (..), IEType (Import), Identifier,
                        IdentifierDetails (..), NodeInfo (..), TypeIndex, nodeInfo)

import qualified Data.Map.Strict as Map
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
        package = showTUnitId $ moduleUnitId $ nameModule name
    in
        isExternalName name
        && occName    == nameMetaName
        && moduleName == nameMetaModuleName
        && ( nameMetaPackage `T.isPrefixOf` package
           -- Cabal hack they made for MacOS. For now, we check for all platforms.
           -- See this issue for more info: https://github.com/kowainik/stan/issues/240
           || withoutVowels nameMetaPackage `T.isPrefixOf` package
           -- Cabal hack they made for Windows. For now, we check for all platforms
           -- See this issue for more info: https://github.com/kowainik/stan/issues/274
           || truncatedWindows nameMetaPackage `T.isPrefixOf` package
           )
  where
    withoutVowels :: Text -> Text
    withoutVowels = T.filter isNotVowel

    isNotVowel :: Char -> Bool
    isNotVowel = \case
        'a' -> False
        'e' -> False
        'i' -> False
        'o' -> False
        'u' -> False
        _ -> True

    truncatedWindows :: Text -> Text
    truncatedWindows s = T.take 13 s <> "_"

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

{- | Check if the given 'HieAST' node is identifier equal to the given
'NameMeta'.
-}
hieFindIdentifier :: NameMeta -> HieAST TypeIndex -> Maybe NameMeta
hieFindIdentifier nameMeta =
    (nameMeta <$)
    . find (hieMatchNameMeta nameMeta)
    . Map.assocs
    . nodeIdentifiers
    . nodeInfo

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

infix 8 `ghcInternalNameFrom`
ghcInternalNameFrom :: Text -> ModuleName -> NameMeta
ghcInternalNameFrom funName moduleName = NameMeta
    { nameMetaName       = funName
    , nameMetaModuleName = moduleName
    , nameMetaPackage    = "ghc-internal"
    }

_nameFrom :: Text -> ModuleName -> NameMeta
#if __GLASGOW_HASKELL__ < 910
_nameFrom = baseNameFrom
#else
_nameFrom = ghcInternalNameFrom
#endif

{- | Create 'NameMeta' for a function from the @base@ package and
the "GHC.List" module.
-}
mkBaseListMeta :: Text -> NameMeta
mkBaseListMeta =
#if __GLASGOW_HASKELL__ < 910
    (`_nameFrom` "GHC.List")
#else
    (`_nameFrom` "GHC.Internal.List")
#endif

{- | Create 'NameMeta' for a function from the @base@ package and
the "Data.OldList" module.
-}
mkBaseOldListMeta :: Text -> NameMeta
mkBaseOldListMeta =
#if __GLASGOW_HASKELL__ < 910
    (`_nameFrom` "Data.OldList")
#else
    (`_nameFrom` "GHC.Internal.Data.OldList")
#endif

{- | Create 'NameMeta' for a function from the @base@ package and
the "Data.Foldable" module.
-}
mkBaseFoldableMeta :: Text -> NameMeta
mkBaseFoldableMeta =
#if __GLASGOW_HASKELL__ < 910
    (`_nameFrom` "Data.Foldable")
#else
    (`_nameFrom` "GHC.Internal.Data.Foldable")
#endif

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

{- | Create 'NameMeta' for a function from the @plutus-tx@ package
and a given 'ModuleName' module.
-}
infix 8 `plutusTxNameFrom`
plutusTxNameFrom :: Text -> ModuleName -> NameMeta
plutusTxNameFrom funName moduleName = NameMeta
    { nameMetaName       = funName
    , nameMetaModuleName = moduleName
    , nameMetaPackage    = "plutus-tx"
    }

{- | Create 'NameMeta' for a function from the @text@ package
and a given 'ModuleName' module.
-}
infix 8 `textNameFrom`
textNameFrom :: Text -> ModuleName -> NameMeta
textNameFrom funName moduleName = NameMeta
    { nameMetaName       = funName
    , nameMetaModuleName = moduleName
    , nameMetaPackage    = "text"
    }

{- | Create 'NameMeta' for a function from the @ghc-prim@ package
and a given 'ModuleName' module.
-}
infix 8 `ghcPrimNameFrom`
ghcPrimNameFrom :: Text -> ModuleName -> NameMeta
ghcPrimNameFrom funName moduleName = NameMeta
    { nameMetaName       = funName
    , nameMetaModuleName = moduleName
    , nameMetaPackage    = "ghc-prim"
    }

-- | 'NameMeta' for primitive types.
primTypeMeta :: Text -> NameMeta
primTypeMeta = (`ghcPrimNameFrom` "GHC.Types")
