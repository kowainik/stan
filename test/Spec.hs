module Main (main) where

import Data.List (find)
import HieTypes (HieFile (..))
import System.Exit (exitFailure)
import Test.Hspec (hspec)

import Stan.Hie (readHieFiles)
import Test.Analysis (analysisSpecs)


main :: IO ()
main = do
    hieFiles <- readHieFiles ".hie"
    case find ((==) "test/Test/Example.hs" . hie_hs_file) hieFiles of
        Just testHie -> hspec $
            analysisSpecs testHie
        Nothing -> do
            putStrLn "FAILED: Example.hie not found"
            () <$ exitFailure
