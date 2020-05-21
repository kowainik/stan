{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Some 'Stan.Inspection.Inspection's require to know about AST and some
mechanism to match parts of syntax tree to the given
'PatternAst'. This information on AST expressions is taken from @HIE
files@ in a more suitable view.

This module contains an implementation of the process of retrieval of this
information from there.
-}

module Stan.Hie.MatchAst
    ( hieMatchPatternAst
    ) where

import HieTypes (HieAST (..), NodeInfo (..), TypeIndex)

import Stan.Core.List (checkWith)
import Stan.NameMeta (hieMatchNameMeta)
import Stan.Pattern.Ast (PatternAst (..))

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


{- | Matching function that matches current AST node with a given
pattern.
-}
hieMatchPatternAst
    :: HieAST TypeIndex
    -> PatternAst
    -> Bool
hieMatchPatternAst Node{..} = \case
    PatternAstAnything -> True
    PatternAstConstant _ -> False  -- TODO: not yet implemented
    PatternAstName nameMeta ->
        any (hieMatchNameMeta nameMeta)
        $ Map.assocs
        $ nodeIdentifiers nodeInfo
    PatternAstNode tags patChildren ->
           tags `Set.isSubsetOf` nodeAnnotations nodeInfo
        && checkWith hieMatchPatternAst nodeChildren patChildren
