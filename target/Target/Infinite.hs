{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Target.Infinite where

import Data.List (genericLength, isSuffixOf)


stanReverse :: [a] -> [a]
stanReverse = reverse

stanIsSuffixOf :: String -> Bool
stanIsSuffixOf = isSuffixOf "stan"

stanGenericLength :: String -> Int
stanGenericLength = genericLength
