{-# LANGUAGE CPP #-}

module Stan.Hie.Compat900
#if __GLASGOW_HASKELL__ == 900
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
    , Stan.Hie.Compat900.DeclType (..)
    , hFunTy2
    , conDec
    , eqDeclType

    , NodeAnnotation
    , mkNodeAnnotation
    , toNodeAnnotation

      -- * Binary interface to hie files
    , HieFileResult (hie_file_result)
    , readHieFileWithNameCache
    , nodeInfo
    ) where

import GHC.Iface.Ext.Binary (HieFileResult (hie_file_result), readHieFile)
import GHC.Iface.Ext.Types
                 (ContextInfo (..), DeclType (..), HieAST (..), HieASTs (..), HieArgs (..),
                 HieFile (..), HieType (..), HieTypeFlat, IEType (..), Identifier,
                 IdentifierDetails (..), NodeInfo (..), TypeIndex,
                 getSourcedNodeInfo)
import GHC.Iface.Ext.Utils (emptyNodeInfo)
import GHC.Types.Name.Cache (initNameCache)
import GHC.Types.Unique.Supply (mkSplitUniqSupply)
import GHC.Data.FastString (FastString)
import GHC.Iface.Env (NameCacheUpdater(NCU))
import GHC.Utils.Outputable (ppr, showSDocUnsafe)

import qualified Data.Map.Strict as Map
import qualified Data.Set as S

import Text.Show (show)

-- This is a direct copy of GHC.Iface.Ext.Utils.emptyNodeInfo except
-- we're using our own redefined combineNodeInfo.
nodeInfo :: Ord a => HieAST a -> NodeInfo a
nodeInfo = foldl' combineNodeInfo emptyNodeInfo . getSourcedNodeInfo . sourcedNodeInfo

-- This is a direct copy of GHC.Iface.Ext.Utils.combineNodeInfo except
-- we use compare rather than nonDetCmpType.
combineNodeInfo :: Ord a => NodeInfo a -> NodeInfo a -> NodeInfo a
(NodeInfo as ai ad) `combineNodeInfo` (NodeInfo bs bi bd) =
  NodeInfo (S.union as bs) (mergeSorted ai bi) (Map.unionWith (<>) ad bd)
  where
    mergeSorted :: Ord b => [b] -> [b] -> [b]
    mergeSorted lc@(c:cs) ld@(d:ds) = case compare c d of
                                        LT -> c : mergeSorted cs ld
                                        EQ -> c : mergeSorted cs ds
                                        GT -> d : mergeSorted lc ds
    mergeSorted cs [] = cs
    mergeSorted [] ds = ds

type NodeAnnotation = (FastString, FastString)

mkNodeAnnotation :: FastString -> FastString -> NodeAnnotation
mkNodeAnnotation = (,)

toNodeAnnotation :: NodeAnnotation -> NodeAnnotation
toNodeAnnotation = id

-- For forward compatibility: the two-argument function type
-- constructor.
hFunTy2 :: HieType b -> Maybe (b, b)
hFunTy2 t = case t of
  HFunTy _multiplicity i1 i2 -> Just (i1, i2)
  _ -> Nothing

readHieFileWithNameCache :: IO (FilePath -> IO HieFileResult)
readHieFileWithNameCache = do
    uniqSupply <- mkSplitUniqSupply 'z'
    let nameCache = initNameCache uniqSupply []
    pure (readHieFile (NCU (\f -> pure $ snd $ f nameCache)))

newtype DeclType = DeclType GHC.Iface.Ext.Types.DeclType
  deriving stock Eq

instance Show Stan.Hie.Compat900.DeclType where
  show (DeclType d) = Text.Show.show (showSDocUnsafe (ppr d))

conDec :: Stan.Hie.Compat900.DeclType
conDec = DeclType ConDec

eqDeclType :: Stan.Hie.Compat900.DeclType -> GHC.Iface.Ext.Types.DeclType -> Bool
eqDeclType (DeclType d1) d2 = d1 == d2
#else
  () where
#endif
