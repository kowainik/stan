{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Target.AntiPattern where

import Data.Foldable (forM_, for_)
import System.FilePath ((</>))

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

stanUrl1 :: FilePath
stanUrl1 = "http://google.com" </> "asd"

stanUrl2 :: FilePath
stanUrl2 = fooUrl </> "asd"
  where
    fooUrl :: FilePath
    fooUrl = "asd"

stanUrl3 :: FilePath
stanUrl3 = "asd" </> fooUrl
  where
    fooUrl :: FilePath
    fooUrl = "asd"

stanUrl4 :: FilePath
stanUrl4 = fooUral </> "asd"
  where
    fooUral :: FilePath
    fooUral = "asd"

stanSlashesUnix :: FilePath
stanSlashesUnix = "asd/asd" </> "xxx"

stanSlashesWindows :: FilePath
stanSlashesWindows = "asd\\asd" </> "xxx"

stanSlashesUnix' :: FilePath
stanSlashesUnix' = "xxx" </> "asd/asd"

stanSlashesWindows' :: FilePath
stanSlashesWindows' = "xxx" </> "asd\\asd"

stanSlashesNo :: FilePath
stanSlashesNo = "xxx" </> "asd"

stanStringsAreBad :: String
stanStringsAreBad = "please never use this data type"

stanFunctionReturningString :: a -> String
stanFunctionReturningString = const "nooo"

newtype Person = Person {name :: String}

stanFunctionTakingString :: String -> Bool
stanFunctionTakingString = const True
