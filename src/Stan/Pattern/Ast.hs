{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Patterns for AST and syntax tree nodes search.
-}

module Stan.Pattern.Ast
    ( -- * Type
      PatternAst (..)

      -- * Helpers
    , namesToPatternAst

      -- * eDSL
    , app
    , constructor
    , dataDecl
    , fixity
    , lazyField
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
    = PatternAstConstant !Int  -- TODO: support constants of different types
    -- | Name of a specific function, variable or data type.
    | PatternAstName !NameMeta !PatternType
    -- | AST node with tags for current node and any children.
    | PatternAstNode
        !(Set (FastString, FastString))  -- ^ Set of context info (pairs of tags)
    -- | AST node with tags for current node and children
    -- patterns. This pattern should match the node exactly.
    | PatternAstNodeExact
        !(Set (FastString, FastString))  -- ^ Set of context info (pairs of tags)
        ![PatternAst]  -- ^ Node children
    -- | AST wildcard, matches anything.
    | PatternAstAnything
    -- | Choice between patterns. Should match either of them.
    | PatternAstOr !PatternAst !PatternAst
    -- | Union of patterns. Should match both of them.
    | PatternAstAnd !PatternAst !PatternAst
    -- | Negation of pattern. Should match everything except this pattern.
    | PatternAstNeg !PatternAst
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

{- | Function that creates 'PatternAst' from the given non-empty list of pairs
'NameMeta' and 'PatternType'.

If the list contains only one 'PatternType' then it is simple 'PatternAstName'.
Else it is 'PatternAstOr' of all such 'PatternAstName's.
-}
namesToPatternAst :: NonEmpty (NameMeta, PatternType) -> PatternAst
namesToPatternAst ((nm, pat) :| []) = PatternAstName nm pat
namesToPatternAst ((nm, pat) :| x:rest) = PatternAstOr
    (PatternAstName nm pat)
    (namesToPatternAst $ x :| rest)

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

{- | Pattern for the function type signature declaration:

@
foo :: Some -> Type
@
-}
typeSig :: PatternAst
typeSig = PatternAstNode $ one ("TypeSig", "Sig")

{- | @data@ or @newtype@ declaration.
-}
dataDecl :: PatternAst
dataDecl = PatternAstNode $ one ("DataDecl", "TyClDecl")

{- | Constructor of a plain data type or newtype. Children of node
that matches this pattern are constructor fields.
-}
constructor :: PatternAst
constructor = PatternAstNode $ one ("ConDeclH98", "ConDecl")

{- | Lazy data type field. Comes in two shapes:

1. Record field, like: @foo :: Text@
2. Simple type: @Int@
-}
lazyField :: PatternAst
lazyField = lazyRecordField ||| type_

{- | Pattern for any occurrence of a plain type. Covers the following
cases:

* Simple type: Int, Bool, a
* Higher-kinded type: Maybe Int, Either String a
* Type in parenthesis: (Int)
* Tuples: (Int, Bool)
* List type: [Int]
* Function type: Int -> Bool
-}
type_ :: PatternAst
type_ =
    PatternAstNode (one ("HsTyVar", "HsType"))  -- simple type: Int, Bool
    |||
    PatternAstNode (one ("HsAppTy", "HsType"))  -- composite: Maybe Int
    |||
    PatternAstNode (one ("HsParTy", "HsType"))  -- type in ()
    |||
    PatternAstNode (one ("HsTupleTy", "HsType"))  -- tuple types: (Int, Bool)
    |||
    PatternAstNode (one ("HsListTy", "HsType"))  -- list types: [Int]
    |||
    PatternAstNode (one ("HsFunTy", "HsType"))  -- function types: Int -> Bool

{- | Pattern for the field without the explicit bang pattern:

@
someField :: Int
@
-}
lazyRecordField :: PatternAst
lazyRecordField = PatternAstNodeExact
    (one ("ConDeclField", "ConDeclField"))
    [ PatternAstNode
        (fromList
            [ ("AbsBinds", "HsBindLR")
            , ("FunBind", "HsBindLR")
            ]
        )
    , type_
    ]
