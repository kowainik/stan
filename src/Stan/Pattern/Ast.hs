{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Patterns for AST and syntax tree nodes search.
-}

module Stan.Pattern.Ast
    ( -- * Type
      PatternAst (..)
    ) where

import FastString (FastString)

import Stan.NameMeta (NameMeta (..))


{- | Query pattern used to search AST nodes in HIE AST. This data type
tries to mirror HIE AST to each future matching, so it's quite
low-level, but helper functions are provided.
-}
data PatternAst
    -- | AST wildcard, matches anything.
    = PatternAstAnything
    -- | Integer constant in code.
    | PatternAstConstant Int  -- TODO: support constants of different types
    -- | Name of a specific function, variable or data type.
    | PatternAstName NameMeta
    -- | AST node with tags for current node and children patterns
    | PatternAstNode
        (Set (FastString, FastString))  -- ^ Set of context info (pairs of tags)
        [PatternAst]  -- ^ Node children
