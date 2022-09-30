{-# LANGUAGE CPP #-}

{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Compatibility module for GHC types and functions. Reexports all
required API to work with the GHC API.
-}

module Stan.Ghc.Compat810
#if __GLASGOW_HASKELL__ <= 810
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

import Avail (AvailInfo (..))
import BasicTypes (PromotionFlag (..), TupleSort (..))
import FastString (FastString, mkFastString)
import FieldLabel (FieldLbl (..))
import IfaceType (IfaceTyCon (..), IfaceTyConInfo (..), IfaceTyConSort (..), IfaceTyLit (..))
import Module (Module, ModuleName, moduleName, moduleNameString, moduleStableString, moduleUnitId,
               UnitId, unitIdString)
import Name (Name, isExternalName, nameModule, nameOccName, nameStableString)
import OccName (isSymOcc, occNameString)
import SrcLoc (RealSrcSpan, srcSpanEndCol, srcSpanEndLine, srcSpanFile, srcSpanStartCol,
               srcSpanStartLine, mkRealSrcLoc, mkRealSrcSpan)
import Var (ArgFlag (..))

import qualified Data.Text as T

showTUnitId :: UnitId -> Text
showTUnitId = T.pack . unitIdString
#else
  where
#endif
