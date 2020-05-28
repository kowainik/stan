{-# LANGUAGE ApplicativeDo #-}

{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

@tomland@ library integration. 'TomlCodec's for the 'Config' data type.
-}

module Stan.Toml
    ( getTomlConfig
      -- * Codecs
    , configCodec
    ) where

import Colourista (infoMessage)
import System.Directory (doesFileExist, getCurrentDirectory, getHomeDirectory)
import System.FilePath ((</>))
import Toml (Key, TomlCodec, (.=))
import Trial (TaggedTrial, Trial (..), fiasco)
import Trial.Tomland (taggedTrialStrCodec)

import Stan.Category (Category (..))
import Stan.Config (Check (..), CheckFilter (..), CheckScope (..), CheckType (..), ConfigP (..),
                    PartialConfig)
import Stan.Core.Id (Id (..))
import Stan.Core.ModuleName (ModuleName (..))
import Stan.Inspection (Inspection (..))
import Stan.Observation (Observation (..))
import Stan.Severity (Severity (..))

import qualified Toml


getTomlConfig :: IO PartialConfig
getTomlConfig = defaultCurConfigFile >>= readToml >>= \case
    Result _ r -> pure r
    resCur -> defaultHomeConfigFile >>= readToml >>= \ resHome ->
        pure $ case resCur <> resHome of
            Fiasco f     -> ConfigP $ Fiasco f
            Result _ res -> res
  where
    readToml :: FilePath -> IO (Trial Text PartialConfig)
    readToml file = do
        isFile <- doesFileExist file
        if isFile
        then do
            infoMessage $ "Reading Configurations from " <> toText file <> " ..."
            pure <$> Toml.decodeFile configCodec file
        else pure $ fiasco $ "TOML Configurations file doesn't exist: " <> toText file

defaultTomlFile :: FilePath
defaultTomlFile = ".stan.toml"

defaultHomeConfigFile :: IO FilePath
defaultHomeConfigFile = (</> defaultTomlFile) <$> getHomeDirectory

defaultCurConfigFile :: IO FilePath
defaultCurConfigFile = (</> defaultTomlFile) <$> getCurrentDirectory

configCodec :: TomlCodec PartialConfig
configCodec = ConfigP
    <$> checksCodec .= configChecks

checksCodec :: TomlCodec (TaggedTrial Text [Check])
checksCodec = do
    checks <- taggedTrialStrCodec (Toml.list checkCodec) "check"
    pure $ case checks of
        Result _ (_, []) -> checks *> fiasco "No TOML value is specified for key 'check'"
        _                -> checks

checkCodec :: TomlCodec Check
checkCodec = Check
    <$> checkTypeCodec .= checkType
    <*> Toml.dioptional checkFilterCodec .= checkFilter
    <*> Toml.dioptional checkScopeCodec  .= checkScope

checkTypeCodec :: TomlCodec CheckType
checkTypeCodec = Toml.enumBounded "type"

----------------------------------------------------------------------------
-- CheckFilter
----------------------------------------------------------------------------

checkInspection :: CheckFilter -> Maybe (Id Inspection)
checkInspection = \case
    CheckInspection idI -> Just idI
    _ -> Nothing

checkObservation :: CheckFilter -> Maybe (Id Observation)
checkObservation = \case
    CheckObservation idO -> Just idO
    _ -> Nothing

checkSeverity :: CheckFilter -> Maybe Severity
checkSeverity = \case
    CheckSeverity sev -> Just sev
    _ -> Nothing

checkCategory :: CheckFilter -> Maybe Category
checkCategory = \case
    CheckCategory category -> Just category
    _ -> Nothing

checkFilterCodec :: TomlCodec CheckFilter
checkFilterCodec =
        Toml.dimatch checkInspection  CheckInspection  (idCodec "inspectionId")
    <|> Toml.dimatch checkObservation CheckObservation (idCodec "observationId")
    <|> Toml.dimatch checkSeverity    CheckSeverity    (Toml.enumBounded "severity")
    <|> Toml.dimatch checkCategory    CheckCategory    (Toml.diwrap (Toml.text "category"))

idCodec :: Key -> TomlCodec (Id a)
idCodec = Toml.diwrap . Toml.text

----------------------------------------------------------------------------
-- CheckScope
----------------------------------------------------------------------------

checkScopeFile :: CheckScope -> Maybe FilePath
checkScopeFile = \case
    CheckScopeFile filePath -> Just filePath
    _ -> Nothing

checkScopeDir :: CheckScope -> Maybe FilePath
checkScopeDir = \case
    CheckScopeDirectory dir -> Just dir
    _ -> Nothing

checkScopeMod :: CheckScope -> Maybe ModuleName
checkScopeMod = \case
    CheckScopeModule m -> Just m
    _ -> Nothing


checkScopeCodec :: TomlCodec CheckScope
checkScopeCodec =
        Toml.dimatch checkScopeFile CheckScopeFile      (Toml.string "file")
    <|> Toml.dimatch checkScopeDir  CheckScopeDirectory (Toml.string "directory")
    <|> Toml.dimatch checkScopeMod  CheckScopeModule    (Toml.diwrap $ Toml.text "module")
