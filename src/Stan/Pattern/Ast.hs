{-# LANGUAGE CPP #-}

{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Patterns for AST and syntax tree nodes search.
-}

module Stan.Pattern.Ast
    ( -- * Type
      PatternAst (..)
    , Literal (..)

      -- * Helpers
    , namesToPatternAst
    , anyNamesToPatternAst

      -- * eDSL
    , app
    , opApp
    , constructor
    , constructorNameIdentifier
    , dataDecl
    , fixity
    , fun
    , guardBranch
    , lazyField
    , range
    , rhs
    , tuple
    , typeSig

      -- * Pattern matching
    , case'
    , lambdaCase
    , patternMatchBranch
    , patternMatchArrow
    , patternMatch_
    , literalPat
    , wildPat

      -- * More low-level interface
    , literalAnns
    ) where

import Stan.Hie.Compat (DeclType, NodeAnnotation, mkNodeAnnotation, conDec)
import Stan.NameMeta (NameMeta (..))
import Stan.Pattern.Edsl (PatternBool (..))
import Stan.Pattern.Type (PatternType)

import qualified Data.Set as Set


{- | Query pattern used to search AST nodes in HIE AST. This data type
tries to mirror HIE AST to each future matching, so it's quite
low-level, but helper functions are provided.
-}
data PatternAst
    -- | Integer constant in code.
    = PatternAstConstant !Literal
    -- | Name of a specific function, variable or data type.
    | PatternAstName !NameMeta !PatternType
    -- | Variable name.
    | PatternAstVarName !String
    -- | AST node with tags for current node and any children.
    | PatternAstNode
        !(Set NodeAnnotation)  -- ^ Set of context info (pairs of tags)
    -- | AST node with tags for current node and children
    -- patterns. This pattern should match the node exactly.
    | PatternAstNodeExact
        !(Set NodeAnnotation)  -- ^ Set of context info (pairs of tags)
        ![PatternAst]  -- ^ Node children
    -- | AST wildcard, matches anything.
    | PatternAstAnything
    -- | Choice between patterns. Should match either of them.
    | PatternAstOr !PatternAst !PatternAst
    -- | Union of patterns. Should match both of them.
    | PatternAstAnd !PatternAst !PatternAst
    -- | Negation of pattern. Should match everything except this pattern.
    | PatternAstNeg !PatternAst
    -- | AST node with the specified Identifier details (only 'DeclType')
    | PatternAstIdentifierDetailsDecl !DeclType
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

data Literal
    = ExactNum !Int
    | ExactStr !ByteString
    | PrefixStr !ByteString
    | ContainStr !ByteString
    | AnyLiteral
    deriving stock (Show, Eq)

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

-- | Like 'namesToPatternAst' but doesn't care about types.
anyNamesToPatternAst :: NonEmpty NameMeta -> PatternAst
anyNamesToPatternAst = namesToPatternAst . fmap (, (?))

-- | @app f x@ is a pattern for function application @f x@.
app :: PatternAst -> PatternAst -> PatternAst
app f x = PatternAstNodeExact (one (mkNodeAnnotation "HsApp" "HsExpr")) [f, x]

-- | @opApp x op y@ is a pattern for operator application @x `op` y@.
opApp :: PatternAst -> PatternAst -> PatternAst -> PatternAst
opApp x op y = PatternAstNodeExact (one (mkNodeAnnotation "OpApp" "HsExpr")) [x, op, y]

-- | @range a b@ is a pattern for @[a .. b]@
range :: PatternAst -> PatternAst -> PatternAst
range from to = PatternAstNodeExact (one (mkNodeAnnotation "ArithSeq" "HsExpr")) [from, to]

-- | 'lambdaCase' is a pattern for @\case@ expression (not considering branches).
lambdaCase :: PatternAst
lambdaCase = PatternAstNode (one (mkNodeAnnotation "HsLamCase" "HsExpr"))

-- | 'case'' is a pattern for @case EXP of@ expression (not considering branches).
case' :: PatternAst
case' = PatternAstNode (one (mkNodeAnnotation "HsCase" "HsExpr"))

-- | Pattern to represent one pattern matching branch.
patternMatchBranch :: PatternAst
patternMatchBranch = PatternAstNode (one (mkNodeAnnotation "Match" "Match"))

{- | Pattern for @_@ in pattern matching.

__Note:__ presents on GHC >=8.10 only.
-}
wildPat :: PatternAst
wildPat = PatternAstNode (one (mkNodeAnnotation "WildPat" "Pat"))

{- | Pattern for literals in pattern matching.

__Note:__ presents on GHC >=8.10 only.
-}
literalPat :: PatternAst
literalPat = PatternAstNode (one (mkNodeAnnotation "NPat" "Pat"))
    ||| PatternAstNode (one (mkNodeAnnotation "LitPat" "Pat"))

-- | Pattern to represent one pattern matching branch on @_@.
patternMatch_ :: PatternAst -> PatternAst
patternMatch_ val = PatternAstNodeExact (one (mkNodeAnnotation "Match" "Match"))
#if __GLASGOW_HASKELL__ >= 810
    $ wildPat :
#endif
    [patternMatchArrow val]

-- | Pattern to represent right side of the pattern matching, e.g. @-> "foo"@.
patternMatchArrow :: PatternAst -> PatternAst
patternMatchArrow x = PatternAstNodeExact (one (mkNodeAnnotation "GRHS" "GRHS")) [x]

{- | Pattern for the top-level fixity declaration:

@
infixr 7 ***, +++, ???
@
-}
fixity :: PatternAst
fixity = PatternAstNode $ one (mkNodeAnnotation "FixitySig" "FixitySig")

{- | Pattern for the function type signature declaration:

@
foo :: Some -> Type
@
-}
typeSig :: PatternAst
typeSig = PatternAstNode $ one (mkNodeAnnotation "TypeSig" "Sig")

absBinds :: NodeAnnotation
absBinds =
#if __GLASGOW_HASKELL__ < 904
  mkNodeAnnotation "AbsBinds" "HsBindLR"
#else
  mkNodeAnnotation "XHsBindsLR" "HsBindLR"
#endif

{- | Pattern for the function definition:

@
foo x y = ...
@
-}
fun :: PatternAst
fun = PatternAstNode $ Set.fromList
    [ absBinds
    , mkNodeAnnotation "FunBind"  "HsBindLR"
    , mkNodeAnnotation "Match"    "Match"
    ]

{- | @data@ or @newtype@ declaration.
-}
dataDecl :: PatternAst
dataDecl = PatternAstNode $ one (mkNodeAnnotation "DataDecl" "TyClDecl")

{- | Constructor of a plain data type or newtype. Children of node
that matches this pattern are constructor fields.
-}
constructor :: PatternAst
constructor = PatternAstNode $ one (mkNodeAnnotation "ConDeclH98" "ConDecl")

{- | Constructor name Identifier info
-}
constructorNameIdentifier :: PatternAst
constructorNameIdentifier = PatternAstIdentifierDetailsDecl conDec

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
    PatternAstNode (one (mkNodeAnnotation "HsTyVar" "HsType"))  -- simple type: Int, Bool
    |||
    PatternAstNode (one (mkNodeAnnotation "HsAppTy" "HsType"))  -- composite: Maybe Int
    |||
    PatternAstNode (one (mkNodeAnnotation "HsParTy" "HsType"))  -- type in ()
    |||
    PatternAstNode (one (mkNodeAnnotation "HsTupleTy" "HsType"))  -- tuple types: (Int, Bool)
    |||
    PatternAstNode (one (mkNodeAnnotation "HsListTy" "HsType"))  -- list types: [Int]
    |||
    PatternAstNode (one (mkNodeAnnotation "HsFunTy" "HsType"))  -- function types: Int -> Bool

{- | Pattern for the field without the explicit bang pattern:

@
someField :: Int
@
-}
lazyRecordField :: PatternAst
lazyRecordField = PatternAstNodeExact
    (one (mkNodeAnnotation "ConDeclField" "ConDeclField"))
    [ PatternAstNode
        (fromList
            [ absBinds
            , mkNodeAnnotation "FunBind" "HsBindLR"
            ]
        )
    , type_
    ]

{- | Pattern for tuples:

* Type signatures: foo :: (Int, Int, Int, Int)
* Literals: (True, 0, [], Nothing)
-}
tuple :: PatternAst
tuple =
    PatternAstNode (one (mkNodeAnnotation "HsTupleTy" "HsType"))  -- tuple type
    |||
    PatternAstNode (one (mkNodeAnnotation "ExplicitTuple" "HsExpr"))  -- tuple literal

{- | Pattern for a single @guard@ branch:

@
    | x < y = ...
@
-}
guardBranch :: PatternAst
guardBranch = PatternAstNode $ one (mkNodeAnnotation "BodyStmt" "StmtLR")

{- | Pattern for the right-hand-side. Usually an equality sign.

@
   foo = baz
@
-}
rhs :: PatternAst
rhs = PatternAstNode $ one (mkNodeAnnotation "GRHS" "GRHS")

-- | Annotations for constants: 0, "foo", etc.
literalAnns :: NodeAnnotation
literalAnns = mkNodeAnnotation "HsOverLit" "HsExpr"
