module Main (main) where

import HieTypes (HieFile (..))
import Test.Hspec (hspec)

import Stan.Hie (readHieFiles)
import Test.Stan.Analysis (analysisSpec)
import Test.Stan.Inspection (inspectionsSpec)
import Test.Stan.Number (linesOfCodeSpec, modulesNumSpec)


main :: IO ()
main = do
    hieFiles <- readHieFiles ".hie"
    case find ((==) "target/Target/Example.hs" . hie_hs_file) hieFiles of
        Just testHie -> hspec $ do
            linesOfCodeSpec testHie
            modulesNumSpec $ length hieFiles
            analysisSpec [testHie]
            inspectionsSpec
        Nothing -> do
            putStrLn "FAILED: Target.Example.hie not found"
            exitFailure
