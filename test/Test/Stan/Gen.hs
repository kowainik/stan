module Test.Stan.Gen
    ( Property
    , genFilePath
    , genId
    , genMediumList
    , genSmallList
    , genSmallString
    , genSmallText

      -- * Config
    , genConfig
    , genCheck
    , genCheckType
    , genCheckFilter
    , genScope
    , genSeverity
    , genCategory
    , genModuleName
    ) where

import Hedgehog (Gen, PropertyT)
import System.FilePath ((</>))

import Stan.Category (Category, stanCategories)
import Stan.Config (Check (..), CheckFilter (..), CheckType (..), Config, ConfigP (..), Scope (..))
import Stan.Core.Id (Id (..))
import Stan.Core.ModuleName (ModuleName (..))
import Stan.Inspection (Inspection)
import Stan.Inspection.All (inspectionsIds)
import Stan.Severity (Severity)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


-- | Helper alias for tests.
type Property = PropertyT IO ()

-- | Generate an 'Int'.
genId :: Gen (Id a)
genId = Id <$> genSmallText

-- | Generate valid 'Inspection' 'Id's.
genInspectionId :: Gen (Id Inspection)
genInspectionId = Gen.element $ toList inspectionsIds

-- | Generate a small list of the given generated elements.
genSmallList :: Gen a -> Gen [a]
genSmallList = Gen.list (Range.linear 0 6)

-- | Generate a medium-size list
genMediumList :: Gen a -> Gen [a]
genMediumList = Gen.list (Range.linear 0 50)

genSmallText :: Gen Text
genSmallText = Gen.text (Range.linear 0 10) Gen.alphaNum

genSmallString :: Gen String
genSmallString = Gen.string (Range.linear 0 10) Gen.alphaNum

genConfig :: Gen Config
genConfig = ConfigP
    <$> genSmallList genCheck
    <*> genSmallList genScope
    <*> genSmallList genId

genCheck :: Gen Check
genCheck = Check
    <$> genCheckType
    <*> genCheckFilter
    <*> genScope

genCheckFilter :: Gen CheckFilter
genCheckFilter = Gen.choice
    [ CheckInspection  <$> genInspectionId
    , CheckSeverity    <$> genSeverity
    , CheckCategory    <$> genCategory
    , pure CheckAll
    ]

genScope :: Gen Scope
genScope = Gen.choice
    [ ScopeFile      <$> genFilePath
    , ScopeDirectory <$> genDirectory
    , pure ScopeAll
    ]

-- | Output one of the hardcoded directories
genDirectory :: Gen FilePath
genDirectory = Gen.element
    [ "src"
    , "app"
    , "lib/"
    , "test/"
    , "benchmark/"
    , "app/main/"
    , "src/app"
    , "src/lib/"
    , "src/lib/foo"
    , "src/lib/bar/"
    ]

-- | Generate a filepath either plain or from a predefined directory
genFilePath :: Gen FilePath
genFilePath = Gen.choice
    [ genSmallString
    , liftA2 (</>) genDirectory genSmallString
    ]

genCheckType :: Gen CheckType
genCheckType = Gen.enumBounded

genSeverity :: Gen Severity
genSeverity = Gen.enumBounded

genCategory :: Gen Category
genCategory = Gen.element stanCategories

genModuleName :: Gen ModuleName
genModuleName = ModuleName <$> genSmallText
