module Test.Stan.Config
    ( configSpec
    ) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Stan.Config (Check (..), CheckFilter (..), CheckType (..), Scope (..), applyChecks,
                    mkDefaultChecks)
import Stan.Core.Id (Id (..))
import Stan.Inspection (Inspection)

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS


configSpec :: Spec
configSpec = describe "Stan Configuration Tests"
    applyChecksSpec

applyChecksSpec :: Spec
applyChecksSpec = describe "applyCheck tests" $ do
    it "All inspections when empty Check list" $
        applyChecks files [] `shouldBe` defMap
    it "Including all inspections" $
        applyChecks files [Check Include CheckAll ScopeAll] `shouldBe` defMap
    it "All inspections are ignored" $
        applyChecks files [Check Ignore CheckAll ScopeAll]
            `shouldBe` (mempty <$ defMap)
    it "Ignoring single Inspection ID works" $ do
        let iId = Id "STAN-0001"
        applyChecks
            files
            [Check Ignore (CheckInspection iId) ScopeAll]
          `shouldBe`
            (HS.delete iId <$> defMap)
    it "Ignoring single file works" $
        applyChecks
            files
            [Check Ignore CheckAll (ScopeFile "baz.hs")]
          `shouldBe`
            HM.adjust (const mempty) "baz.hs" defMap
    it "Ignoring a directory works" $
        applyChecks
            files
            [Check Ignore CheckAll (ScopeDirectory "src/")]
          `shouldBe`
            ( HM.adjust (const mempty) "src/foo.hs"
            $ HM.adjust (const mempty) "src/bar.hs" defMap)
    it "Ignoring inspection in a single file" $ do
        let iId = Id "STAN-0001"
        applyChecks
            files
            [Check
                Ignore
                (CheckInspection iId)
                (ScopeFile "baz.hs")
            ]
          `shouldBe`
            HM.adjust (HS.delete iId) "baz.hs" defMap
  where
    files :: [FilePath]
    files = ["src/foo.hs", "src/bar.hs", "baz.hs"]

    defMap :: HashMap FilePath (HashSet (Id Inspection))
    defMap = mkDefaultChecks files
