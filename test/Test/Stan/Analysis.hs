module Test.Stan.Analysis
    ( analysisSpec
    ) where

import Stan.Hie.Compat (HieFile (..))
import System.FilePath ((</>))
import Test.Hspec (Spec, describe, it, runIO, shouldBe)

import Stan (createCabalExtensionsMap)
import Stan.Analysis (Analysis (..), runAnalysis)
import Stan.Config (mkDefaultChecks)
import Stan.Core.Id (Id (..))
import Stan.Observation (Observation (..))
import Test.Stan.Analysis.AntiPattern (analysisAntiPatternSpec)
import Test.Stan.Analysis.PlutusTx (analysisPlutusTxSpec)
import Test.Stan.Analysis.Infinite (analysisInfiniteSpec)
import Test.Stan.Analysis.Partial (analysisPartialSpec)
import Test.Stan.Analysis.Style (analysisStyleSpec)

import qualified Data.Set as Set
--import qualified GHC.Prelude as Prel
--import GHC.IO (unsafePerformIO)
--import Text.Pretty.Simple (pPrint)


analysisSpec :: [HieFile] -> Spec
analysisSpec hieFiles = describe "Static Analysis" $ do
    extensionsMap <- runIO $ createCabalExtensionsMap True ["stan.cabal"] hieFiles
    let checksMap = mkDefaultChecks (map hie_hs_file hieFiles)

    -- tests without ignorance
    --(Just myFile) = find ((== "target/Target/PlutusTx.hs") . hie_hs_file) hieFiles
    let analysis = runAnalysis extensionsMap checksMap [] hieFiles
    analysisPartialSpec analysis
    analysisInfiniteSpec analysis
    analysisAntiPatternSpec analysis
    analysisPlutusTxSpec analysis
    analysisStyleSpec analysis
    analysisExtensionsSpec analysis

    -- testing with ignoring observations
    let analyseWithIgnored ignoredObs = runAnalysis
            extensionsMap
            checksMap
            ignoredObs
            -- running analysis on a single file to speed up tests
            [hieFileByName $ "target" </> "Target" </> "Partial.hs"]
    analysisIgnoredObservationsSpec analyseWithIgnored
  where
    hieFileByName :: FilePath -> HieFile
    hieFileByName path = case filter ((== path) . hie_hs_file) hieFiles of
        []  -> error $ "Invalid test file path: " <> toText path
        h:_ -> h

analysisExtensionsSpec :: Analysis -> Spec
analysisExtensionsSpec Analysis{..} = describe "Used extensions" $ do
    it "should correctly count total amount of used extensions" $
        Set.size (fst analysisUsedExtensions) `shouldBe` 25
    it "should correctly count total amount of used safe extensions" $
        Set.size (snd analysisUsedExtensions) `shouldBe` 0

analysisIgnoredObservationsSpec
    :: ([Id Observation] -> Analysis)  -- ^ Run analysis with ignored observations
    -> Spec
analysisIgnoredObservationsSpec analyse = describe "Ignores observations" $ do
    let obsId = Id "OBS-STAN-0001-pnvTKA-17:12"

    it "ObservationId is present when not ignored " $ do
        let observationsIds = fmap observationId $ analysisObservations $ analyse []
        find (== obsId) observationsIds `shouldBe` Just obsId

    it "ObservationId is properly ignored " $ do
        let observations = analysisObservations $ analyse [obsId]
        find ((== obsId) . observationId) observations `shouldBe` Nothing
