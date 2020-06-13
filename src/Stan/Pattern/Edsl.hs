{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Embedded DSL for patterns (AST and Type). Implemented using the
/Final Taggless/ approach.
-}

module Stan.Pattern.Edsl
    ( PatternBool (..)
    ) where


{- | Common interface for 'Bool'-like parts of patterns. Allows to
write composable and reusable complex pattern definitions from smaller
pieces.

Laws (in terms of matching functions that return 'Bool'):

* @'(?)' ≡ 'True'@
* @'neg' '(?)' ≡ 'False'@
* @'(?)' '|||' x ≡ x '|||' '(?)' ≡ 'True'@
* @'(?)' '&&&' x ≡ x '&&&' '(?)' ≡ x@
-}
class PatternBool a where
    -- | Anything. Matching should always return 'True'.
    (?) :: a

    -- | Negation. Inverses the argument.
    neg :: a -> a

    -- | Or-pattern. Choice.
    (|||) :: a -> a -> a

    -- | And-pattern. Both.
    (&&&) :: a -> a -> a

infixr 2 |||
infixr 3 &&&
