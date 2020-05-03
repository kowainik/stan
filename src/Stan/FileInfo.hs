{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

File (or module) specific information.
-}

module Stan.FileInfo
    ( FileMap
    , FileInfo (..)
    ) where

import Extensions (ExtensionsError, ExtensionsResult, ParsedExtensions)

import Stan.Observation (Observations)


-- | File specific information.
data FileInfo = FileInfo
    { fileInfoPath             :: !FilePath
    , fileInfoLoc              :: !Int
    , fileInfoCabalExtensions  :: !(Either ExtensionsError ParsedExtensions)
    , fileInfoExtensions       :: !(Either ExtensionsError ParsedExtensions)
    , fileInfoMergedExtensions :: !ExtensionsResult
    , fileInfoObservations     :: !Observations
    } deriving stock (Show, Eq)

type FileMap = Map FilePath FileInfo
