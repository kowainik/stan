{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Target.AntiPattern where

import Data.Foldable (forM_, for_)


import qualified Data.ByteString.Char8 as BS8
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List as List
import qualified Data.Text as Text


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

stanHashMapLength :: HM.HashMap Int Int -> Int
stanHashMapLength = length

stanHashSetSize :: HS.HashSet Int -> Int
stanHashSetSize = HS.size

stanHashSetLength :: HS.HashSet Int -> Int
stanHashSetLength = length

stanTupleLength :: Int
stanTupleLength = length ((1, 2) :: (Int, Int))

stanMaybeNull :: Maybe Int -> Bool
stanMaybeNull = null

stanEitherFoldr :: Either Int Int -> Int
stanEitherFoldr = foldr (+) 0

stanTextLength :: Text.Text -> Int
stanTextLength = Text.length

stanNub :: [Int] -> [Int]
stanNub = List.nub

stanFor_ :: IO ()
stanFor_ = for_ [1 :: Int .. 1000] print

stanForM_ :: Int -> IO ()
stanForM_ n = forM_ [1 .. n] print
