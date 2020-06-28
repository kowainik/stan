{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Compatibility module for HIE types from GHC API. Reexports all
required API to work with HIE types.
-}

module Stan.Hie.Compat
    ( -- * Main HIE types
      ContextInfo (..)
    , HieArgs (..)
    , HieAST (..)
    , HieASTs (..)
    , HieFile (..)
    , HieType (..)
    , HieTypeFlat
    , IEType (..)
    , Identifier
    , IdentifierDetails (..)
    , NodeInfo (..)
    , TypeIndex
    , DeclType (..)

      -- * Binary interface to hie files
    , HieFileResult (hie_file_result)
    , readHieFile

      -- * Name cache to read HIE files
    , NameCache
    , initNameCache
    , mkSplitUniqSupply
    ) where

import HieBin (HieFileResult (hie_file_result), readHieFile)
import HieTypes (ContextInfo (..), DeclType (..), HieAST (..), HieASTs (..), HieArgs (..),
                 HieFile (..), HieType (..), HieTypeFlat, IEType (..), Identifier,
                 IdentifierDetails (..), NodeInfo (..), TypeIndex)
import NameCache (NameCache, initNameCache)
import UniqSupply (mkSplitUniqSupply)
