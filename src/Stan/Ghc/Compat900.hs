{-# LANGUAGE CPP #-}

{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Compatibility module for GHC types and functions. Reexports all
required API to work with the GHC API.
-}

module Stan.Ghc.Compat900
#if __GLASGOW_HASKELL__ == 900
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
    , FieldLbl (..)
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
import GHC.Types.FieldLabel (FieldLbl (..))
import GHC.Iface.Type (IfaceTyCon (..), IfaceTyConInfo (..), IfaceTyConSort (..), IfaceTyLit (..))
import GHC.Unit.Types (Module, moduleName)
import GHC.Unit.Module (moduleStableString)
import GHC.Unit (moduleUnit, toUnitId, UnitId, unitIdString)
import GHC.Unit.Module.Name (ModuleName, moduleNameString)
import GHC.Types.Name (Name, isExternalName, nameModule, nameOccName, nameStableString)
import GHC.Types.Name.Occurrence (isSymOcc, occNameString)
import GHC.Types.SrcLoc (RealSrcSpan, srcSpanEndCol, srcSpanEndLine, srcSpanFile, srcSpanStartCol,
                         srcSpanStartLine, mkRealSrcSpan, mkRealSrcLoc)
import GHC.Types.Var (ArgFlag (..))

import qualified Data.Text as T

moduleUnitId :: Module -> UnitId
moduleUnitId = toUnitId . moduleUnit

showTUnitId :: UnitId -> Text
showTUnitId = T.pack . unitIdString
#else
  () where
#endif
