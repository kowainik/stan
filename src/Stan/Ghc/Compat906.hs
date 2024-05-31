{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Stan.Ghc.Compat906
#if __GLASGOW_HASKELL__ == 906 || __GLASGOW_HASKELL__ == 908
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
    , AvailInfo (..)
    , FastString
    , mkFastString
    , FieldLbl
    , FieldLabel (..)
    , FieldLabelString (..)
    , ForAllTyFlag (..)
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
import Language.Haskell.Syntax.Module.Name (ModuleName, moduleNameString)
import Language.Haskell.Syntax.Basic (FieldLabelString(..))
import GHC.Types.Name (Name, isExternalName, nameModule, nameOccName, nameStableString)
import GHC.Types.Name.Occurrence (isSymOcc, occNameString)
import GHC.Types.SrcLoc (RealSrcSpan, srcSpanEndCol, srcSpanEndLine, srcSpanFile, srcSpanStartCol,
                         srcSpanStartLine, mkRealSrcSpan, mkRealSrcLoc)
import GHC.Types.Var (ForAllTyFlag (..), Specificity (..))

moduleUnitId :: Module -> UnitId
moduleUnitId = toUnitId . moduleUnit

showTUnitId :: UnitId -> Text
showTUnitId = toText . unitIdString

type FieldLbl = FieldLabel

deriving stock instance Show Specificity => Show ForAllTyFlag

deriving stock instance Show FieldLabelString

#else
  () where
#endif
