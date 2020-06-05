{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Patterns for AST and syntax tree nodes search.
-}

module Stan.Pattern.Ast
    ( -- * Type
      PatternAst (..)

      -- * eDSL
    , app
    , fixity
    , range
    , typeSig
    ) where

import FastString (FastString)

import Stan.NameMeta (NameMeta (..))
import Stan.Pattern.Edsl (PatternBool (..))
import Stan.Pattern.Type (PatternType)


{- | Query pattern used to search AST nodes in HIE AST. This data type
tries to mirror HIE AST to each future matching, so it's quite
low-level, but helper functions are provided.
-}
data PatternAst
    -- | Integer constant in code.
    = PatternAstConstant Int  -- TODO: support constants of different types
    -- | Name of a specific function, variable or data type.
    | PatternAstName NameMeta PatternType
    -- | AST node with tags for current node and any children.
    | PatternAstNode
        (Set (FastString, FastString))  -- ^ Set of context info (pairs of tags)
    -- | AST node with tags for current node and children
    -- patterns. This pattern should match the node exactly.
    | PatternAstNodeExact
        (Set (FastString, FastString))  -- ^ Set of context info (pairs of tags)
        [PatternAst]  -- ^ Node children
    -- | AST wildcard, matches anything.
    | PatternAstAnything
    -- | Choice between patterns. Should match either of them.
    | PatternAstOr PatternAst PatternAst
    -- | Union of patterns. Should match both of them.
    | PatternAstAnd PatternAst PatternAst
    -- | Negation of pattern. Should match everything except this pattern.
    | PatternAstNeg PatternAst
    deriving stock (Show, Eq)

instance PatternBool PatternAst where
    (?) :: PatternAst
    (?) = PatternAstAnything

    neg :: PatternAst -> PatternAst
    neg = PatternAstNeg

    (|||) :: PatternAst -> PatternAst -> PatternAst
    (|||) = PatternAstOr

    (&&&) :: PatternAst -> PatternAst -> PatternAst
    (&&&) = PatternAstAnd

-- | @app f x@ is a pattern for function application @f x@.
app :: PatternAst -> PatternAst -> PatternAst
app f x = PatternAstNodeExact (one ("HsApp", "HsExpr")) [f, x]

-- | @range a b@ is a pattern for @[a .. b]@
range :: PatternAst -> PatternAst -> PatternAst
range from to = PatternAstNodeExact (one ("ArithSeq", "HsExpr")) [from, to]

{- | Pattern for the top-level fixity declaration:

@
infixr 7 ***, +++, ???
@
-}
fixity :: PatternAst
fixity = PatternAstNode $ one ("FixitySig", "FixitySig")

{- | Pattern for a type signature declaration:

@
foo :: Some -> Type
@
-}
typeSig :: PatternAst
typeSig = PatternAstNode $ one ("TypeSig", "Sig")
