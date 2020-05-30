module Test.Stan.Gen
    ( Property
    , genId
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

import Stan.Category (Category, stanCategories)
import Stan.Config (Check (..), CheckFilter (..), CheckType (..), Config, ConfigP (..), Scope (..))
import Stan.Core.Id (Id (..))
import Stan.Core.ModuleName (ModuleName (..))
import Stan.Severity (Severity)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


-- | Helper alias for tests.
type Property = PropertyT IO ()

-- | Generate an 'Int'.
genId :: Gen (Id a)
genId = Id <$> genSmallText

-- | Generate a small list of the given generated elements.
genSmallList :: Gen a -> Gen [a]
genSmallList = Gen.list (Range.linear 0 6)

genSmallText :: Gen Text
genSmallText = Gen.text (Range.linear 0 10) Gen.alphaNum

genSmallString :: Gen String
genSmallString = Gen.string (Range.linear 0 10) Gen.alphaNum

genConfig :: Gen Config
genConfig = ConfigP
    <$> genSmallList genCheck
    <*> genSmallList genScope

genCheck :: Gen Check
genCheck = Check
    <$> genCheckType
    <*> genCheckFilter
    <*> genScope

genCheckFilter :: Gen CheckFilter
genCheckFilter = Gen.choice
    [ CheckInspection  <$> genId
    , CheckObservation <$> genId
    , CheckSeverity    <$> genSeverity
    , CheckCategory    <$> genCategory
    , pure CheckAll
    ]

genScope :: Gen Scope
genScope = Gen.choice
    [ ScopeFile      <$> genSmallString
    , ScopeDirectory <$> genSmallString
    , pure ScopeAll
    ]

genCheckType :: Gen CheckType
genCheckType = Gen.enumBounded

genSeverity :: Gen Severity
genSeverity = Gen.enumBounded

genCategory :: Gen Category
genCategory = Gen.element stanCategories

genModuleName :: Gen ModuleName
genModuleName = ModuleName <$> genSmallText
