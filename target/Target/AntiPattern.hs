{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Target.AntiPattern where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS


stanLengthXs :: [a] -> [Int]
stanLengthXs xs = [0 .. length xs]

stanLengthXsMinus1 :: [a] -> [Int]
stanLengthXsMinus1 xs = [0 .. length xs - 1]

stanFoldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
stanFoldl = foldl

stanPack8 :: String -> BS8.ByteString
stanPack8 = BS8.pack

stanHashMapSize :: HM.HashMap Int Int -> Int
stanHashMapSize = HM.size

stanHashSetSize :: HS.HashSet Int -> Int
stanHashSetSize = HS.size
