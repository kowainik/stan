{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Extra functions to work with lists.
-}

module Stan.Core.List
    ( checkWith
    , nonRepeatingPairs
    ) where

{- | Checks that two lists have the same length and that a given
binary predicate returns 'True' on each corresponding pair of
elements.

>>> checkWith (==) [] []
True
>>> checkWith (==) [1, 2] [1, 2]
True
>>> checkWith (==) [1, 2] [2, 1]
False
>>> checkWith (==) [1, 2] [1]
False
-}
checkWith :: (a -> b -> Bool) -> [a] -> [b] -> Bool
checkWith _ [] []         = True
checkWith _ [] _          = False
checkWith _ _ []          = False
checkWith f (a:as) (b:bs) = f a b && checkWith f as bs

{- | Returns list all element pairs without the following properties:

* No element with itself: @(x, x)@
* Only one of @(x, y)@ and @(y, x)@ will be in the result

In other words, it's like taking the cross product of the list with
itself and then keeping only the half of that without the diagonal.

>>> foo [1..3]
[(1, 2), (1, 3), (2, 3)]
-}
nonRepeatingPairs :: [a] -> [(a, a)]
nonRepeatingPairs []     = []
nonRepeatingPairs (x:xs) = map (x,) xs ++ nonRepeatingPairs xs
