module Test.Stan.Cli
    ( cliSpec
    ) where

import Options.Applicative (ParserResult (Success), execParserPure)
import Test.Hspec (Spec, describe, it, shouldBe)
import Trial (withTag)

import Stan.Cli (StanArgs (..), StanCommand (..), stanCliParser, stanParserPrefs)
import Stan.Config (Check (..), CheckFilter (..), CheckScope (..), CheckType (..), Config,
                    ConfigP (..), PartialConfig, configToCliCommand)
import Stan.Core.Id (Id (..))


cliSpec :: Spec
cliSpec = describe "CLI configuration tests" $ do
    it "Converts Config to CLI command and parses it back" $
        case execParserPure stanParserPrefs stanCliParser stanCommand of
            Success (Stan StanArgs{..}) -> stanArgsConfig `shouldBe` partialConfigExample
            _                           -> fail "Toml-To-CLI doesn't match the original config"

  where
    configExample :: Config
    configExample = ConfigP
        { configChecks = checks
        }

    partialConfigExample :: PartialConfig
    partialConfigExample = ConfigP
        { configChecks = withTag "CLI" $ pure checks
        }

    checks :: [Check]
    checks =
        [ Check Ignore Nothing (Just $ CheckScopeDirectory "test/")
        , Check Include Nothing Nothing
        , Check Ignore (Just $ CheckInspection $ Id "STAN-0002") Nothing
        , Check
            Ignore
            (Just $ CheckInspection $ Id "STAN-0001")
            (Just $ CheckScopeFile "src/MyFile.hs")
        ]

    stanCommand :: [String]
    stanCommand = filter (/= "\\") $ drop 1 $ map toString $ words $ configToCliCommand configExample
