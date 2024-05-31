{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Target.Infinite where

import Data.List (genericLength, isSuffixOf)

import qualified GHC.List


stanReverse :: [a] -> [a]
stanReverse = reverse

stanIsSuffixOf :: String -> Bool
stanIsSuffixOf = isSuffixOf "stan"

stanLength :: String -> Int
stanLength = length

stanGenericLength :: String -> Int
stanGenericLength = genericLength

stanSum :: Num a => [a] -> a
stanSum = sum

stanProduct :: Num a => [a] -> a
stanProduct = product

stanGhcListxxLength :: [a] -> Int
stanGhcListxxLength = GHC.List.length
