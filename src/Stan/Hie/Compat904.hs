{-# LANGUAGE CPP #-}

module Stan.Hie.Compat904
#if __GLASGOW_HASKELL__ == 904 || __GLASGOW_HASKELL__ == 906 || __GLASGOW_HASKELL__ == 908
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
    , Stan.Hie.Compat904.DeclType (..)
    , hFunTy2
    , conDec
    , eqDeclType
    , Stan.Hie.Compat904.NodeAnnotation
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
                 getSourcedNodeInfo, NodeAnnotation(..))
import GHC.Iface.Ext.Utils (emptyNodeInfo)
import GHC.Types.Name.Cache (initNameCache)
import GHC.Data.FastString (FastString)
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

mkNodeAnnotation :: FastString
                 -> FastString
                 -> Stan.Hie.Compat904.NodeAnnotation
mkNodeAnnotation f1 f2 =
  Stan.Hie.Compat904.NodeAnnotation (GHC.Iface.Ext.Types.NodeAnnotation f1 f2)

newtype NodeAnnotation = NodeAnnotation GHC.Iface.Ext.Types.NodeAnnotation
  deriving stock (Eq, Ord)

instance Show Stan.Hie.Compat904.NodeAnnotation where
  show
    (Stan.Hie.Compat904.NodeAnnotation (GHC.Iface.Ext.Types.NodeAnnotation a1 a2)) =
    Text.Show.show (a1, a2)

toNodeAnnotation :: GHC.Iface.Ext.Types.NodeAnnotation
                 -> Stan.Hie.Compat904.NodeAnnotation
toNodeAnnotation = Stan.Hie.Compat904.NodeAnnotation

-- For forward compatibility: the two-argument function type
-- constructor.
hFunTy2 :: HieType b -> Maybe (b, b)
hFunTy2 t = case t of
  HFunTy _multiplicity i1 i2 -> Just (i1, i2)
  _ -> Nothing

readHieFileWithNameCache :: IO (FilePath -> IO HieFileResult)
readHieFileWithNameCache = do
    nameCache <- initNameCache 'z' []
    pure (readHieFile nameCache)

newtype DeclType = DeclType GHC.Iface.Ext.Types.DeclType
  deriving stock Eq

instance Show Stan.Hie.Compat904.DeclType where
  show (DeclType d) = Text.Show.show (showSDocUnsafe (ppr d))

conDec :: Stan.Hie.Compat904.DeclType
conDec = DeclType ConDec

eqDeclType :: Stan.Hie.Compat904.DeclType -> GHC.Iface.Ext.Types.DeclType -> Bool
eqDeclType (DeclType d1) d2 = d1 == d2
#else
  () where
#endif

