{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Target.Infinite where

import Data.List (genericLength, isSuffixOf)


stanReverse :: [a] -> [a]
stanReverse = reverse

stanIsSuffixOf :: String -> Bool
stanIsSuffixOf = isSuffixOf "stan"

stanLength :: [a] -> Int
stanLength = length

stanGenericLength :: String -> Int
stanGenericLength = genericLength

stanSum :: Num a => [a] -> a
stanSum = sum

stanProduct :: Num a => [a] -> a
stanProduct = product
