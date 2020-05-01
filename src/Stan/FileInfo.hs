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

import Extensions (ExtensionsResult)
import Extensions.OnOff (OnOffExtension)

import Stan.Observation (Observations)


-- | File specific information.
data FileInfo = FileInfo
    { fileInfoPath            :: !FilePath
    , fileInfoLoc             :: !Int
    , fileInfoCabalExtensions :: !ExtensionsResult
    , fileInfoExtensions      :: !ExtensionsResult
    , fileMergedExtensions    :: !(Set OnOffExtension)
    , fileInfoObservations    :: !Observations
    } deriving stock (Show, Eq)

type FileMap = Map FilePath FileInfo
