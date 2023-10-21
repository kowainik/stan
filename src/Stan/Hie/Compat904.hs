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
                 IdentifierDetails (..), NodeInfo (..), TypeIndex, NodeOrigin(SourceInfo, GeneratedInfo),
                 getSourcedNodeInfo, NodeAnnotation(..))
import GHC.Types.Name.Cache (initNameCache)
import GHC.Data.FastString (FastString)
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
