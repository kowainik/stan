module Main (main) where

import Data.List (find)
import HieTypes (HieFile (..))
import System.Exit (exitFailure)
import Test.Hspec (hspec)

import Stan.Hie (readHieFiles)
import Test.Analysis (linesOfCodeSpec, modulesNumSpec)


main :: IO ()
main = do
    hieFiles <- readHieFiles ".hie"
    case find ((==) "test/Test/Example.hs" . hie_hs_file) hieFiles of
        Just testHie -> hspec $ do
            linesOfCodeSpec testHie
            modulesNumSpec $ length hieFiles
        Nothing -> do
            putStrLn "FAILED: Example.hie not found"
            () <$ exitFailure
