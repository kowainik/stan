{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Target.Partial where

import Data.Foldable (maximumBy, minimumBy)
import Data.List (foldl1', genericIndex)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromJust)
import GHC.Exts (fromList)
import Numeric.Natural (Natural)

import qualified Data.List.NonEmpty as NE


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

stanPred :: Natural -> Natural
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

stanFromList :: [x] -> NonEmpty x
stanFromList = fromList

stanFromInteger :: Integer -> Natural
stanFromInteger = fromInteger

-- Other tests

stanSuccNatural :: Natural -> Natural
stanSuccNatural = succ  -- no warning here

stanPredInteger :: Integer -> Integer
stanPredInteger = pred  -- no warning here

stanPredPoly :: Enum a => a -> a
stanPredPoly = pred

stanFromListNE :: [x] -> NonEmpty x
stanFromListNE = NE.fromList
