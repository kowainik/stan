{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Target.Partial where

import Data.Foldable (maximumBy, minimumBy)
import Data.List (foldl1', genericIndex)
import Data.Maybe (fromJust)


stanHead :: [a] -> a
stanHead = head

stanTail :: [a] -> [a]
stanTail = tail

stanInit :: [a] -> [a]
stanInit = init

stanLast :: [a] -> a
stanLast = last

stanAt :: [a] -> a
stanAt xs = xs !! 42

stanCycle :: [a] -> [a]
stanCycle = cycle

stanGenericIndex :: [a] -> Int -> a
stanGenericIndex = genericIndex

stanFromJust :: Maybe Int -> Int
stanFromJust = fromJust

stanRead :: String -> Int
stanRead = read

stanSucc :: Int -> Int
stanSucc = succ

stanPred :: Int -> Int
stanPred = pred

stanToEnum :: Int -> Bool
stanToEnum = toEnum

stanMaximum :: [Int] -> Int
stanMaximum = maximum

stanMinimum :: Ord a => [a] -> a
stanMinimum = minimum

stanMaximumBy :: [Int] -> Int
stanMaximumBy = maximumBy compare

stanMinimumBy :: (a -> a -> Ordering) -> [a] -> a
stanMinimumBy = minimumBy

stanFoldl1 :: (a -> a -> a) -> [a] -> a
stanFoldl1 = foldl1

stanFoldl1' :: (a -> a -> a) -> [a] -> a
stanFoldl1' = foldl1'

stanFoldr1 :: (a -> a -> a) -> [a] -> a
stanFoldr1 = foldr1
