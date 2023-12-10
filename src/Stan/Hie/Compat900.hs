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
                 IdentifierDetails (..), NodeInfo (..), TypeIndex, NodeOrigin(SourceInfo, GeneratedInfo),
                 getSourcedNodeInfo)
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
nodeInfo :: Ord a => HieAST a -> NodeInfo a
nodeInfo h = case (lookup' SourceInfo, lookup' GeneratedInfo) of
  (Nothing, Nothing) -> error "nodeInfo"
  (Just n1, Nothing) -> n1
  (Nothing, Just{}) -> error "nodeInfo"
  (Just n1, Just{}) -> n1
  where lookup' k = Map.lookup k (getSourcedNodeInfo (sourcedNodeInfo h))

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
