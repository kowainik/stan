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
    ) where

import Extensions (ExtensionsError, ExtensionsResult, ParsedExtensions (..), showOnOffExtension)

import Stan.Core.ModuleName (ModuleName)
import Stan.Observation (Observations)


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
