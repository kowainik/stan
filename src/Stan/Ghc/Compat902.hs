{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Stan.Ghc.Compat902
#if __GLASGOW_HASKELL__ == 902 || __GLASGOW_HASKELL__ == 904
    ( -- * Modules
      Module
    , ModuleName
    , moduleNameString
    , moduleName
    , moduleStableString
    , moduleUnitId

      -- * Names
    , Name
    , isExternalName
    , isSymOcc
    , nameModule
    , nameOccName
    , nameStableString
    , occNameString

      -- * Source locations
    , RealSrcSpan
    , srcSpanEndCol
    , srcSpanStartCol
    , srcSpanStartLine
    , srcSpanEndLine
    , srcSpanFile
    , mkRealSrcLoc
    , mkRealSrcSpan

      -- * Other common types (for debugging and not only)
    , ArgFlag (..)
    , AvailInfo (..)
    , FastString
    , mkFastString
    , FieldLbl
    , FieldLabel (..)
    , IfaceTyCon (..)
    , IfaceTyConInfo (..)
    , IfaceTyConSort (..)
    , IfaceTyLit (..)
    , PromotionFlag (..)
    , TupleSort (..)
    , showTUnitId
    ) where

import GHC.Types.Avail (AvailInfo (..))
import GHC.Types.Basic (PromotionFlag (..), TupleSort (..))
import GHC.Data.FastString (FastString, mkFastString)
import GHC.Types.FieldLabel (FieldLabel (..))
import GHC.Iface.Type (IfaceTyCon (..), IfaceTyConInfo (..), IfaceTyConSort (..), IfaceTyLit (..))
import GHC.Unit.Types (Module, moduleName)
import GHC.Unit.Module (moduleStableString)
import GHC.Unit (moduleUnit, toUnitId, UnitId, unitIdString)
import GHC.Unit.Module.Name (ModuleName, moduleNameString)
import GHC.Types.Name (Name, isExternalName, nameModule, nameOccName, nameStableString)
import GHC.Types.Name.Occurrence (isSymOcc, occNameString)
import GHC.Types.SrcLoc (RealSrcSpan, srcSpanEndCol, srcSpanEndLine, srcSpanFile, srcSpanStartCol,
                         srcSpanStartLine, mkRealSrcSpan, mkRealSrcLoc)
import GHC.Types.Var (ArgFlag (..), Specificity (..))

import qualified Data.Text as T

moduleUnitId :: Module -> UnitId
moduleUnitId = toUnitId . moduleUnit

showTUnitId :: UnitId -> Text
showTUnitId = T.pack . unitIdString

type FieldLbl = FieldLabel

deriving stock instance Show Specificity => Show ArgFlag

#else
  () where
#endif
