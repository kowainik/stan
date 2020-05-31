module Test.Stan.Config
    ( configSpec
    ) where

import Hedgehog (forAll, (===))
import Test.Hspec (Spec, describe, it, parallel, shouldBe)
import Test.Hspec.Hedgehog (hedgehog)

import Stan.Config (Check (..), CheckFilter (..), CheckType (..), Config, ConfigP (..), Scope (..),
                    applyChecks, applyChecksFor, applyConfig, mkDefaultChecks)
import Stan.Core.Id (Id (..))
import Stan.Inspection (Inspection)
import Test.Stan.Gen (Property, genCheck, genFilePath, genMediumList, genSmallList)

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS


configSpec :: Spec
configSpec = describe "Stan Configuration Tests"
    applyChecksSpec

applyChecksSpec :: Spec
applyChecksSpec = describe "Configuration checks aggregation" $ do
    applyChecksUnitSpec
    applyChecksPropertySpec
    applyConfigSpec

applyChecksUnitSpec :: Spec
applyChecksUnitSpec = describe "applyCheck: Unit Tests" $ do
    it "All inspections when empty Check list" $
        applyChecks files [] `shouldBe` defMap
    it "Including all inspections" $
        applyChecks files [Check Include CheckAll ScopeAll] `shouldBe` defMap
    it "All inspections are excluded" $
        applyChecks files [Check Exclude CheckAll ScopeAll]
            `shouldBe` (mempty <$ defMap)
    it "Excluding single Inspection ID works" $ do
        let iId = Id "STAN-0001"
        applyChecks
            files
            [Check Exclude (CheckInspection iId) ScopeAll]
          `shouldBe`
            (HS.delete iId <$> defMap)
    it "Excluding single file works" $
        applyChecks
            files
            [Check Exclude CheckAll (ScopeFile "baz.hs")]
          `shouldBe`
            HM.adjust (const mempty) "baz.hs" defMap
    it "Excluding a directory works" $
        applyChecks
            files
            [Check Exclude CheckAll (ScopeDirectory "src/")]
          `shouldBe`
              HM.adjust (const mempty) "src/foo.hs"
            ( HM.adjust (const mempty) "src/bar.hs" defMap)
    it "Excluding inspection in a single file" $ do
        let iId = Id "STAN-0001"
        applyChecks
            files
            [Check
                Exclude
                (CheckInspection iId)
                (ScopeFile "baz.hs")
            ]
          `shouldBe`
            HM.adjust (HS.delete iId) "baz.hs" defMap

applyChecksPropertySpec :: Spec
applyChecksPropertySpec = describe "applyCheck: Property Tests" $ parallel $ do
    it "Idempotence: applyChecks . applyChecks ≡ applyChecks"
        idempotenceProperty
    it "CheckType Inversion: include c . exclude c ≡ id"
        inversionProperty

idempotenceProperty :: Property
idempotenceProperty = hedgehog $ do
    paths  <- forAll $ genSmallList genFilePath
    checks <- forAll $ genMediumList genCheck

    let filesMap = mkDefaultChecks paths

    applyChecksFor (applyChecksFor filesMap checks) checks === applyChecksFor filesMap checks

inversionProperty :: Property
inversionProperty = hedgehog $ do
    paths <- forAll $ genSmallList genFilePath
    excludeCheck <- forAll $ genCheck <&> \c -> c { checkType = Exclude }
    let includeCheck = excludeCheck { checkType = Include }

    let filesMap = mkDefaultChecks paths
    applyChecksFor (applyChecksFor filesMap [excludeCheck]) [includeCheck] === filesMap

applyConfigSpec :: Spec
applyConfigSpec = describe "applyConfig tests" $ do
    it "'applyConfig' with no removed files is the same as 'applyChecks'" $ do
        let checks = [Check Exclude CheckAll ScopeAll]
        applyConfig files (emptyConfig{ configChecks = checks })
           `shouldBe` applyChecks files checks
    it "Removes all files" $
        applyConfig files removeAllConfig `shouldBe` mempty
    it "Removes a single file" $
        applyConfig files removeFileConfig
          `shouldBe` HM.delete "baz.hs" defMap
    it "Removes all files from a directory" $
        applyConfig files removeDirectoryConfig
          `shouldBe` HM.delete "src/foo.hs" (HM.delete "src/bar.hs" defMap)
    it "Removes 2 files" $
        applyConfig files removeTwoFilesConfig
          `shouldBe` HM.delete "src/foo.hs" (HM.delete "baz.hs" defMap)
    it "Removes all files when they are specified explicitly" $
        applyConfig files removeAllExplicitConfig `shouldBe` mempty
  where
    emptyConfig :: Config
    emptyConfig = ConfigP
        { configChecks = []
        , configRemoved = []
        , configIgnored = []
        }

    removeAllConfig :: Config
    removeAllConfig = emptyConfig { configRemoved = [ScopeAll] }

    removeFileConfig :: Config
    removeFileConfig = emptyConfig { configRemoved = [ScopeFile "baz.hs"] }

    removeDirectoryConfig :: Config
    removeDirectoryConfig = emptyConfig { configRemoved = [ScopeDirectory "src/"] }

    removeTwoFilesConfig :: Config
    removeTwoFilesConfig = emptyConfig
        { configRemoved = [ScopeFile "baz.hs", ScopeFile "src/foo.hs"]
        }

    removeAllExplicitConfig :: Config
    removeAllExplicitConfig = emptyConfig
        { configRemoved = [ScopeFile "baz.hs", ScopeDirectory "src"]
        }

files :: [FilePath]
files = ["src/foo.hs", "src/bar.hs", "baz.hs"]

defMap :: HashMap FilePath (HashSet (Id Inspection))
defMap = mkDefaultChecks files
