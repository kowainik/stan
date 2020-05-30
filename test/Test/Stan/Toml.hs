module Test.Stan.Toml
    ( tomlSpec
    ) where

import Hedgehog (forAll, (===))
import Test.Hspec (Spec, describe, it, shouldReturn)
import Test.Hspec.Hedgehog (hedgehog)
import Trial (Trial (..), fiasco, withTag)

import Stan.Config (Check (..), CheckFilter (..), CheckType (..), Config, ConfigP (..),
                    PartialConfig, Scope (..), finaliseConfig)
import Stan.Core.Id (Id (..))
import Stan.Toml (configCodec)
import Test.Stan.Gen (Property, genConfig)

import qualified Toml


tomlSpec :: Spec
tomlSpec = describe "TOML configuration tests" $ do
    it "decode . encode ≡ pure" configTomlRoundtripProperty
    it "Parses test/.stan.example.toml" $
        Toml.decodeFile configCodec "test/.stan-example.toml" `shouldReturn` configExample
  where
    configExample :: PartialConfig
    configExample = ConfigP
        { configChecks = withTag "TOML" $ pure
            [ Check Ignore Nothing (Just $ ScopeDirectory "test/")
            , Check Include Nothing Nothing
            , Check Ignore (Just $ CheckInspection $ Id "STAN-0002") Nothing
            , Check
                Ignore
                (Just $ CheckInspection $ Id "STAN-0001")
                (Just $ ScopeFile "src/MyFile.hs")
            ]
        , configRemoved = withTag "TOML" $ pure [] <> fiasco "No TOML value is specified for key: remove"
        }

configTomlRoundtripProperty :: Property
configTomlRoundtripProperty = hedgehog $ do
    config <- forAll genConfig
    case (Toml.decode configCodec $ Toml.encode configCodec $ toPartialConfig config) of
        Right partialConfig -> case finaliseConfig partialConfig of
            Result _ res -> config === res
            _            -> fail "Expected Result, but this is Fiasco, bro"
        _                   -> fail "Expected Right"

toPartialConfig :: Config -> PartialConfig
toPartialConfig ConfigP{..} = ConfigP
    { configChecks = withTag "TOML" $ pure configChecks
    , configRemoved = withTag "TOML" $ pure configRemoved
    }
