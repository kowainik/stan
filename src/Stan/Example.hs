{- HLINT ignore "Unused LANGUAGE pragma" -}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

{-# LANGUAGE NoUndecidableInstances #-}
{-# LANGUAGE PatternSynonyms        #-}

{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Module that contains code that should trigger various beautiful Stan inspections :)
-}

module Stan.Example () where

import System.FilePath ((</>))

import qualified Data.ByteString.Char8 as BS8
import qualified Data.List as P
import qualified Text.Read as P


mkMyUrlPart :: String -> String
mkMyUrlPart myUrl = myUrl </> "asd"

pairLength :: Int
pairLength = length ((1, 2) :: (Int, Int))

listIndxs :: [a] -> [Int]
listIndxs xs = [0 .. length xs]

toByteString :: String -> BS8.ByteString
toByteString = BS8.pack

pathToX :: FilePath -> FilePath
pathToX x = "src/lib" </> x

getFirstIPromise :: [a] -> a
getFirstIPromise = P.head

parseInt :: String -> Int
parseInt = P.read

quad :: Int -> (Int, Int, Int, Int)
quad x = (x, x, x, x)

(?+?) :: Int -> Int -> Int
(?+?) = (+)

isEq :: Int -> Int -> Bool
isEq x y
    | x < y = False
    | x > y = False
    | otherwise = True

prettyOrdering :: Ordering -> String
prettyOrdering = \case {GT -> "GT"; _ -> "LT"}

data User = User
    { userName :: !String
    , userAge  :: Int
    }
