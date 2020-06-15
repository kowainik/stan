{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Compatibility module for GHC types and functions. Reexports all
required API to work with the GHC API.
-}

module Stan.Ghc.Compat
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

      -- * Other common types (for debugging and not only)
    , ArgFlag (..)
    , AvailInfo (..)
    , FastString
    , FieldLbl (..)
    , IfaceTyCon (..)
    , IfaceTyConInfo (..)
    , IfaceTyConSort (..)
    , IfaceTyLit (..)
    , PromotionFlag (..)
    , TupleSort (..)
    ) where

import Avail (AvailInfo (..))
import BasicTypes (PromotionFlag (..), TupleSort (..))
import FastString (FastString)
import FieldLabel (FieldLbl (..))
import IfaceType (IfaceTyCon (..), IfaceTyConInfo (..), IfaceTyConSort (..), IfaceTyLit (..))
import Module (Module, ModuleName, moduleName, moduleNameString, moduleStableString, moduleUnitId)
import Name (Name, isExternalName, nameModule, nameOccName, nameStableString)
import OccName (isSymOcc, occNameString)
import SrcLoc (RealSrcSpan, srcSpanEndCol, srcSpanEndLine, srcSpanFile, srcSpanStartCol,
               srcSpanStartLine)
import Var (ArgFlag (..))
