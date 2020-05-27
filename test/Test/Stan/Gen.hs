module Test.Stan.Gen
    ( Property
    , genId
    , genSmallList
    , genSmallString
    , genSmallText
    ) where

import Hedgehog (Gen, PropertyT)

import Stan.Core.Id (Id (..))

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
