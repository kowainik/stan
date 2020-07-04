{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

File (or module) specific information.
-}

module Stan.FileInfo
    ( FileMap
    , FileInfo (..)

    , extensionsToText
    , isExtensionDisabled
    ) where

import Data.Aeson.Micro (ToJSON (..), object, (.=))
import Extensions (Extensions (..), ExtensionsError, ExtensionsResult, OnOffExtension (..),
                   ParsedExtensions (..), showOnOffExtension)
import GHC.LanguageExtensions.Type (Extension)

import Stan.Core.ModuleName (ModuleName)
import Stan.Observation (Observations)

import qualified Data.Set as Set


-- | File specific information.
data FileInfo = FileInfo
    { fileInfoPath             :: !FilePath
    , fileInfoModuleName       :: !ModuleName
    , fileInfoLoc              :: !Int
    , fileInfoCabalExtensions  :: !(Either ExtensionsError ParsedExtensions)
    , fileInfoExtensions       :: !(Either ExtensionsError ParsedExtensions)
    , fileInfoMergedExtensions :: !ExtensionsResult
    , fileInfoObservations     :: !Observations
    } deriving stock (Show, Eq)

instance ToJSON FileInfo where
    toJSON FileInfo{..} = object
        [ "path"            .= toText fileInfoPath
        , "moduleName"      .= fileInfoModuleName
        , "loc"             .= fileInfoLoc
        , "cabalExtensions" .= extensionsToText fileInfoCabalExtensions
        , "extensions"      .= extensionsToText fileInfoExtensions
        , "observations"    .= toList fileInfoObservations
        ]

type FileMap = Map FilePath FileInfo

-- | Return the list of pretty-printed extensions.
extensionsToText :: Either ExtensionsError ParsedExtensions -> [Text]
extensionsToText = \case
    Left _ -> ["Unable to extract extensions"]
    Right ParsedExtensions{..} ->
        let exts = map showOnOffExtension parsedExtensionsAll in
        case parsedExtensionsSafe of
            Just s  -> show s : exts
            Nothing -> exts

{- | Check whether the given extension is disabled
-}
isExtensionDisabled :: Extension -> ExtensionsResult -> Bool
isExtensionDisabled ext = \case
    Left _ -> True  -- no info about extensions, consider it disabled
    Right Extensions{..} ->
           Set.notMember (On ext) extensionsAll
        || Set.member (Off ext) extensionsAll
