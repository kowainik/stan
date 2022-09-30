module Main (main) where

import Stan.Hie.Compat (HieFile (..))
import System.FilePath ((</>))
import Test.Hspec (hspec)

import Stan.Hie (readHieFiles)
import Test.Stan.Analysis (analysisSpec)
import Test.Stan.Cli (cliSpec)
import Test.Stan.Config (configSpec)
import Test.Stan.Number (linesOfCodeSpec, modulesNumSpec)
import Test.Stan.Observation (observationSpec)
import Test.Stan.Toml (tomlSpec)


main :: IO ()
main = do
    hieFiles <- readHieFiles ".hie"
    case filter isTargetFile hieFiles of
        [] -> do
            putStrLn "FAILED: target/ files are not found"
            exitFailure
        testHies -> do
            Just exampleHie <- pure $
                find ((==) ("target" </> "Target" </> "Partial.hs") . hie_hs_file) testHies
            hspec $ do
                linesOfCodeSpec exampleHie
                modulesNumSpec $ length hieFiles
                cliSpec
                tomlSpec
                configSpec
                observationSpec
                analysisSpec testHies

isTargetFile :: HieFile -> Bool
isTargetFile HieFile{..} = "target" `isPrefixOf` hie_hs_file
