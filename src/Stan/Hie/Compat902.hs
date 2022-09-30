{-# LANGUAGE CPP #-}

module Stan.Hie.Compat902
#if __GLASGOW_HASKELL__ == 902
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
    , Stan.Hie.Compat902.DeclType (..)
    , hFunTy2
    , conDec
    , eqDeclType
    , Stan.Hie.Compat902.NodeAnnotation
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
                 IdentifierDetails (..), NodeInfo (..), TypeIndex, NodeOrigin(SourceInfo, GeneratedInfo),
                 getSourcedNodeInfo, NodeAnnotation(..))
import GHC.Types.Name.Cache (initNameCache)
import GHC.Types.Unique.Supply (mkSplitUniqSupply)
import GHC.Data.FastString (FastString)
import GHC.Iface.Env (NameCacheUpdater(NCU))
import GHC.Utils.Outputable (ppr, showSDocUnsafe)

import qualified Data.Map.Strict as Map

import Text.Show (show)

-- It's not clear if this is completely correct, or whether
--
-- 1. we should merge in the GeneratedInfo, and/or
-- 2. return a NodeInfo with empty fields when the SourceInfo is empty
--
-- It works though.
nodeInfo :: HieAST a -> NodeInfo a
nodeInfo h = case (lookup' SourceInfo, lookup' GeneratedInfo) of
  (Nothing, Nothing) -> error "nodeInfo"
  (Just n1, Nothing) -> n1
  (Nothing, Just{}) -> error "nodeInfo"
  (Just n1, Just{}) -> n1
  where lookup' k = Map.lookup k (getSourcedNodeInfo (sourcedNodeInfo h))

mkNodeAnnotation :: FastString
                 -> FastString
                 -> Stan.Hie.Compat902.NodeAnnotation
mkNodeAnnotation f1 f2 =
  Stan.Hie.Compat902.NodeAnnotation (GHC.Iface.Ext.Types.NodeAnnotation f1 f2)

data NodeAnnotation = NodeAnnotation GHC.Iface.Ext.Types.NodeAnnotation
  deriving stock Eq

-- This is a horrendous hack for 9.2
-- Fixed later by ghc 418295eab741fd420c6f350141c332ef26f9f0a4 I think
instance Ord Stan.Hie.Compat902.NodeAnnotation where
  compare
    (Stan.Hie.Compat902.NodeAnnotation (GHC.Iface.Ext.Types.NodeAnnotation a1 a2))
    (Stan.Hie.Compat902.NodeAnnotation (GHC.Iface.Ext.Types.NodeAnnotation b1 b2)) =
      let s = Text.Show.show
      in compare (s a1) (s b1) <> compare (s a2) (s b2)

instance Show Stan.Hie.Compat902.NodeAnnotation where
  show
    (Stan.Hie.Compat902.NodeAnnotation (GHC.Iface.Ext.Types.NodeAnnotation a1 a2)) =
    Text.Show.show (a1, a2)

toNodeAnnotation :: GHC.Iface.Ext.Types.NodeAnnotation
                 -> Stan.Hie.Compat902.NodeAnnotation
toNodeAnnotation = Stan.Hie.Compat902.NodeAnnotation

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

instance Show Stan.Hie.Compat902.DeclType where
  show (DeclType d) = Text.Show.show (showSDocUnsafe (ppr d))

conDec :: Stan.Hie.Compat902.DeclType
conDec = DeclType ConDec

eqDeclType :: Stan.Hie.Compat902.DeclType -> GHC.Iface.Ext.Types.DeclType -> Bool
eqDeclType (DeclType d1) d2 = d1 == d2
#else
  () where
#endif
