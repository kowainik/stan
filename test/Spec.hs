module Main (main) where

import HieTypes (HieFile (..))
import Test.Hspec (hspec)

import Stan.Hie (readHieFiles)
import Test.Analysis (analysisSpec)
import Test.Number (linesOfCodeSpec, modulesNumSpec)


main :: IO ()
main = do
    hieFiles <- readHieFiles ".hie"
    case find ((==) "target/Target/Example.hs" . hie_hs_file) hieFiles of
        Just testHie -> hspec $ do
            linesOfCodeSpec testHie
            modulesNumSpec $ length hieFiles
            analysisSpec [testHie]
        Nothing -> do
            putStrLn "FAILED: Target.Example.hie not found"
            exitFailure
