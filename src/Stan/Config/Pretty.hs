{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

@stan@ configuration pretty printing helper functions.
-}

module Stan.Config.Pretty
    ( ConfigAction (..)
    , prettyConfigAction
    , configActionClass
    , configActionColour
    , prettyConfigCli

    , configToTriples
    ) where

import Colourista (bold, formatWith, green, magenta, red, yellow)

import Stan.Category (Category (..))
import Stan.Config (Check (..), CheckFilter (..), CheckType (..), Config, ConfigP (..), Scope (..))
import Stan.Core.Id (Id (..))


data ConfigAction
    = RemoveAction
    | IncludeAction
    | ExcludeAction
    | IgnoreAction
    deriving stock (Show, Eq)

prettyConfigAction :: ConfigAction -> Text
prettyConfigAction = \case
    RemoveAction  -> "— Remove "
    IncludeAction -> "∪ Include"
    ExcludeAction -> "∩ Exclude"
    IgnoreAction  -> "✖ Ignore "

configActionClass :: ConfigAction -> Text
configActionClass = \case
    RemoveAction  -> "remove"
    IncludeAction -> "include"
    ExcludeAction -> "exclude"
    IgnoreAction  -> "ignore"

configActionColour :: ConfigAction -> Text
configActionColour = \case
    RemoveAction  -> red
    IncludeAction -> green
    ExcludeAction -> yellow
    IgnoreAction  -> magenta

configToTriples :: Config -> [(ConfigAction, Text, Text)]
configToTriples ConfigP{..} =
       map ((RemoveAction, "", ) . prettyScope) configRemoved
    ++ map checkToTriple configChecks
    ++ map ((IgnoreAction, , "") . unId) configIgnored

checkToTriple :: Check -> (ConfigAction, Text, Text)
checkToTriple Check{..} =
    ( checkTypeToAction checkType
    , prettyFilter checkFilter
    , prettyScope checkScope
    )

checkTypeToAction :: CheckType -> ConfigAction
checkTypeToAction = \case
    Include -> IncludeAction
    Exclude -> ExcludeAction

prettyFilter :: CheckFilter -> Text
prettyFilter = \case
    CheckInspection ins -> "ID: " <> unId ins
    CheckSeverity sev -> "Severity: " <> show sev
    CheckCategory cat -> "Category: " <> unCategory cat
    CheckAll -> "All inspections"

prettyScope :: Scope -> Text
prettyScope = \case
    ScopeFile fp -> "File: " <> toText fp
    ScopeDirectory dir -> "Directory: " <> toText dir
    ScopeAll -> "All files"

prettyConfigCli :: Config -> Text
prettyConfigCli = unlines . concatMap action . configToTriples
  where
    action :: (ConfigAction, Text, Text) -> [Text]
    action (act, check, scope) =
          formatWith [configActionColour act, bold] (prettyConfigAction act)
        :  [ "    " <> check | check /= ""]
        ++ [ "    " <> scope | scope /= ""]
