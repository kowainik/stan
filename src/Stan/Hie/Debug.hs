{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE StandaloneDeriving #-}

{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Useful debugging and printing utilities for HIE types. They are
implemented in two ways:

1. Using derived 'Show' instances.
2. Using @ghc@ pretty-printing.

To make full use of derived 'Show' instances, add the @pretty-simple@
package to dependencies and use the @pPrint@ function from the
@Text.Pretty.Simple@ module.
-}

module Stan.Hie.Debug
    ( debugHieFile
    , debugDynFlags
    , printHieAsts
    , printSDoc
    ) where

import Avail (AvailInfo (..))
import BasicTypes (PromotionFlag (..), TupleSort (..))
import DynFlags (DynFlags, getDynFlags)
import FieldLabel (FieldLbl (..))
import GHC (runGhc)
import GHC.Paths (libdir)
import HieDebug (ppHies)
import HieTypes (HieAST (..), HieASTs (..), HieArgs (..), HieFile (..), HieType (..),
                 IdentifierDetails (..), NodeInfo (..))
import IfaceType (IfaceTyCon (..), IfaceTyConInfo (..), IfaceTyConSort (..), IfaceTyLit (..))
import Module (Module, ModuleName, moduleNameString, moduleStableString)
import Name (Name, nameStableString)
import Outputable (CodeStyle (CStyle), SDoc, mkCodeStyle, printSDocLn)
import Pretty (Mode (PageMode))
import Text.Pretty.Simple (pPrint)
import Var (ArgFlag (..))

import qualified Text.Show


debugHieFile :: FilePath -> [HieFile] -> IO ()
debugHieFile path hieFiles = do
    let mHieFile = find (\HieFile{..} -> hie_hs_file == path) hieFiles
    whenJust mHieFile pPrint

debugDynFlags :: IO DynFlags
debugDynFlags = runGhc (Just libdir) getDynFlags

{- | Prints 'HieASTs' part of 'HieFile' using @ghc@ pretty-printing.
-}
printHieAsts :: HieFile -> IO ()
printHieAsts hieFile = do
    dynFlags <- debugDynFlags
    printSDoc dynFlags (ppHies $ hie_asts hieFile)

printSDoc :: DynFlags -> SDoc -> IO ()
printSDoc dynFlags doc = printSDocLn
    PageMode
    dynFlags
    stdout
    (mkCodeStyle CStyle)
    doc

-- orphan intances
deriving stock instance Show HieFile
deriving stock instance Show a => Show (HieType a)
deriving stock instance Show a => Show (HieAST a)
deriving newtype instance Show a => Show (HieASTs a)
deriving newtype instance Show a => Show (HieArgs a)
deriving stock instance Show a => Show (NodeInfo a)
deriving stock instance Show a => Show (IdentifierDetails a)
deriving stock instance Show IfaceTyCon
deriving stock instance Show IfaceTyConInfo
deriving stock instance Show IfaceTyConSort
deriving stock instance Show IfaceTyLit
deriving stock instance Show PromotionFlag
deriving stock instance Show TupleSort
deriving stock instance Show ArgFlag
deriving stock instance Show AvailInfo
deriving stock instance Show a => Show (FieldLbl a)

instance Show Module where
    show = moduleStableString

instance Show ModuleName where
    show = moduleNameString

instance Show Name where
    show = nameStableString
