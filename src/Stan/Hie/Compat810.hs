{-# LANGUAGE CPP #-}

{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Compatibility module for HIE types from GHC API. Reexports all
required API to work with HIE types.
-}

module Stan.Hie.Compat810
#if __GLASGOW_HASKELL__ <= 810
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
    , hFunTy2
    , conDec
    , eqDeclType

    , NodeAnnotation
    , mkNodeAnnotation
    , toNodeAnnotation

      -- * Binary interface to hie files
    , HieFileResult (hie_file_result)
    , readHieFileWithNameCache
    ) where

import HieBin (HieFileResult (hie_file_result), readHieFile)
import HieTypes (ContextInfo (..), DeclType (..), HieAST (..), HieASTs (..), HieArgs (..),
                 HieFile (..), HieType (..), HieTypeFlat, IEType (..), Identifier,
                 IdentifierDetails (..), NodeInfo (..), TypeIndex)
import NameCache (initNameCache)
import UniqSupply (mkSplitUniqSupply)
import FastString (FastString)

type NodeAnnotation = (FastString, FastString)

mkNodeAnnotation :: FastString -> FastString -> NodeAnnotation
mkNodeAnnotation = (,)

toNodeAnnotation :: NodeAnnotation -> NodeAnnotation
toNodeAnnotation = id

-- For forward compatibility: the two-argument function type
-- constructor.
hFunTy2 :: HieType b -> Maybe (b, b)
hFunTy2 t = case t of
  HFunTy i1 i2 -> Just (i1, i2)
  _ -> Nothing

readHieFileWithNameCache :: IO (FilePath -> IO HieFileResult)
readHieFileWithNameCache = do
    uniqSupply <- mkSplitUniqSupply 'z'
    let nameCache = initNameCache uniqSupply []
    pure (fmap fst . readHieFile nameCache)

conDec :: DeclType
conDec = ConDec

eqDeclType :: DeclType -> DeclType -> Bool
eqDeclType d1 d2 = d1 == d2
#else
  () where
#endif
