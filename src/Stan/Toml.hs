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
import Toml (AnyValue, BiMap (..), Key, TomlBiMap, TomlCodec, (.=))
import Trial (TaggedTrial, Trial (..), fiasco)
import Trial.Tomland (taggedTrialStrCodec)

import Stan.Category (Category (..))
import Stan.Config (Check (..), CheckFilter (..), CheckType (..), ConfigP (..), PartialConfig,
                    Scope (..))
import Stan.Core.Id (Id (..))
import Stan.Inspection (Inspection (..))
import Stan.Observation (Observation (..))
import Stan.Severity (Severity (..))

import qualified Toml


getTomlConfig :: Bool -> Maybe FilePath -> IO PartialConfig
getTomlConfig useDefault mTomlFile = do
    def <-
        if useDefault
        then defaultCurConfigFile >>= readToml >>= \case
            Result _ r -> pure r
            resCur -> defaultHomeConfigFile >>= readToml >>= \ resHome ->
                pure $ inline $ resCur <> resHome
        else let e = fiasco "Selected NOT to use any default .stan.toml configuration files"
             in pure $ ConfigP e e e
    case mTomlFile of
        Just tomlFile -> (def <>) . inline <$> readToml tomlFile
        Nothing       -> pure def
  where
    readToml :: FilePath -> IO (Trial Text PartialConfig)
    readToml file = do
        isFile <- doesFileExist file
        if isFile
        then do
            infoMessage $ "Reading Configurations from " <> toText file <> " ..."
            pure <$> Toml.decodeFile configCodec file
        else pure $ fiasco $ "TOML Configurations file doesn't exist: " <> toText file

    inline :: Trial Text PartialConfig -> PartialConfig
    inline = \case
        Fiasco f     -> let e = Fiasco f in ConfigP e e e
        Result _ res -> res

defaultTomlFile :: FilePath
defaultTomlFile = ".stan.toml"

defaultHomeConfigFile :: IO FilePath
defaultHomeConfigFile = (</> defaultTomlFile) <$> getHomeDirectory

defaultCurConfigFile :: IO FilePath
defaultCurConfigFile = (</> defaultTomlFile) <$> getCurrentDirectory

configCodec :: TomlCodec PartialConfig
configCodec = ConfigP
    <$> checksCodec  .= configChecks
    <*> removedCodec .= configRemoved
    <*> ignoredCodec .= configIgnored

removedCodec :: TomlCodec (TaggedTrial Text [Scope])
removedCodec = taggedTrialListCodec "remove" scopeCodec

ignoredCodec :: TomlCodec (TaggedTrial Text [Id Observation])
ignoredCodec = taggedTrialListCodec "ignore" idCodec

checksCodec :: TomlCodec (TaggedTrial Text [Check])
checksCodec = taggedTrialListCodec "check" checkCodec

taggedTrialListCodec :: Key -> TomlCodec a -> TomlCodec (TaggedTrial Text [a])
taggedTrialListCodec key aCodec = do
    res <- taggedTrialStrCodec (Toml.list aCodec) key
    pure $ case res of
        Result _ (_, []) -> res <> fiasco ("No TOML value is specified for key: " <> Toml.prettyKey key)
        _                -> res

checkCodec :: TomlCodec Check
checkCodec = Check
    <$> checkTypeCodec   .= checkType
    <*> checkFilterCodec .= checkFilter
    <*> scopeCodec       .= checkScope

checkTypeCodec :: TomlCodec CheckType
checkTypeCodec = Toml.enumBounded "type"

----------------------------------------------------------------------------
-- CheckFilter
----------------------------------------------------------------------------

checkInspection :: CheckFilter -> Maybe (Id Inspection)
checkInspection = \case
    CheckInspection idI -> Just idI
    _ -> Nothing

checkSeverity :: CheckFilter -> Maybe Severity
checkSeverity = \case
    CheckSeverity sev -> Just sev
    _ -> Nothing

checkCategory :: CheckFilter -> Maybe Category
checkCategory = \case
    CheckCategory category -> Just category
    _ -> Nothing

checkAll :: CheckFilter -> Maybe ()
checkAll = \case
    CheckAll -> Just ()
    _ -> Nothing

checkFilterCodec :: TomlCodec CheckFilter
checkFilterCodec =
        Toml.dimatch checkInspection CheckInspection  idCodec
    <|> Toml.dimatch checkSeverity   CheckSeverity    (Toml.enumBounded "severity")
    <|> Toml.dimatch checkCategory   CheckCategory    (Toml.diwrap (Toml.text "category"))
    <|> Toml.dimatch checkAll        (const CheckAll) (allCodec "filter")

idCodec :: TomlCodec (Id a)
idCodec = Toml.diwrap $ Toml.text "id"

----------------------------------------------------------------------------
-- CheckScope
----------------------------------------------------------------------------

scopeFile :: Scope -> Maybe FilePath
scopeFile = \case
    ScopeFile filePath -> Just filePath
    _ -> Nothing

scopeDir :: Scope -> Maybe FilePath
scopeDir = \case
    ScopeDirectory dir -> Just dir
    _ -> Nothing

scopeAll :: Scope -> Maybe ()
scopeAll = \case
    ScopeAll -> Just ()
    _ -> Nothing

scopeCodec :: TomlCodec Scope
scopeCodec =
        Toml.dimatch scopeFile ScopeFile        (Toml.string "file")
    <|> Toml.dimatch scopeDir  ScopeDirectory   (Toml.string "directory")
    <|> Toml.dimatch scopeAll  (const ScopeAll) (allCodec "scope")

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Helper 'BiMap' for the hardcoded string @"all"@.
_All :: TomlBiMap () AnyValue
_All = _AllText >>> Toml._Text
  where
    _AllText :: TomlBiMap () Text
    _AllText = BiMap
        { forward  = \() -> Right "all"
        , backward = \case
            "all" -> Right ()
            t -> Left $ Toml.ArbitraryError $ "Expected Text value \"all\" but got: " <> t
        }

allCodec :: Key -> TomlCodec ()
allCodec = Toml.match _All
