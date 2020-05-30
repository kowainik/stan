module Test.Stan.Cli
    ( cliSpec
    ) where

import Hedgehog (forAll, (===))
import Options.Applicative (ParserResult (Success), execParserPure)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Hedgehog (hedgehog)
import Trial (fiasco, trialToMaybe, withTag)

import Stan.Cli (StanArgs (..), StanCommand (..), stanCliParser, stanParserPrefs)
import Stan.Config (Check (..), CheckFilter (..), CheckType (..), Config, ConfigP (..),
                    PartialConfig, Scope (..), configToCliCommand, finaliseConfig)
import Stan.Core.Id (Id (..))
import Test.Stan.Gen (Property, genCheck, genConfig, genId, genScope)


cliSpec :: Spec
cliSpec = describe "CLI configuration tests" $ do
    it "execParserPure . configToCliCommand ≡ pure" cliConfigRoundtripProperty
    it "Converts Config to CLI command and parses it back" $
        case execParserPure stanParserPrefs stanCliParser (stanCommand configExample) of
            Success (Stan StanArgs{..}) -> stanArgsConfig `shouldBe` partialConfigExample
            _                           -> fail "Toml-To-CLI doesn't match the original config"

  where
    configExample :: Config
    configExample = ConfigP
        { configChecks = checks
        , configRemoved = []
        , configObservations = []
        }

    partialConfigExample :: PartialConfig
    partialConfigExample = ConfigP
        { configChecks = withTag "CLI" $ pure checks
        , configRemoved = withTag "CLI" $ fiasco "No CLI option specified for: remove"
        , configObservations = withTag "CLI" $ fiasco "No CLI option specified for: observation"
        }

    checks :: [Check]
    checks =
        [ Check Ignore CheckAll (ScopeDirectory "test/")
        , Check Include CheckAll ScopeAll
        , Check Ignore (CheckInspection $ Id "STAN-0002") ScopeAll
        , Check
            Ignore
            (CheckInspection $ Id "STAN-0001")
            (ScopeFile "src/MyFile.hs")
        ]

cliConfigRoundtripProperty :: Property
cliConfigRoundtripProperty = hedgehog $ do
    check <- forAll genCheck
    scope <- forAll genScope
    obs   <- forAll genId
    -- we need to have a non-empty list as empty list is Fiasco for CLI.
    config <- forAll $ genConfig <&> \c -> c
        { configChecks  = check : configChecks c
        , configRemoved = scope : configRemoved c
        , configObservations = obs : configObservations c
        }
    case execParserPure stanParserPrefs stanCliParser (stanCommand config) of
        Success (Stan StanArgs{..}) ->
            trialToMaybe (finaliseConfig stanArgsConfig) === Just config
        _ -> fail $ "Toml-To-CLI doesn't match the original config:\n"
            <> toString (configToCliCommand config)

stanCommand :: Config -> [String]
stanCommand = filter (/= "\\") . drop 1 . map toString . words . configToCliCommand
