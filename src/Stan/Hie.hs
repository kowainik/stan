{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Functions to work with @hie@ specific parts.
-}

module Stan.Hie
    ( readHieFiles
    , countLinesOfCode
    ) where

import HieBin (HieFileResult (hie_file_result), readHieFile)
import HieTypes (HieFile (..))
import NameCache (NameCache, initNameCache)
import System.Directory (doesFileExist)
import System.Directory.Recursive (getDirRecursive)
import System.FilePath (takeExtension)
import UniqSupply (mkSplitUniqSupply)

import Stan.Hie.Debug ()

import qualified Data.ByteString.Char8 as BS8


{- | Returns contents of all @.hie@ files recursively in the given
@hie@ directory.
-}
readHieFiles :: FilePath -> IO [HieFile]
readHieFiles hieDir = do
    nameCache <- createNameCache
    hieContent <- getDirRecursive hieDir
    let isHieFile f = (&&) (takeExtension f == ".hie") <$> doesFileExist f
    hieFiles <- filterM isHieFile hieContent
    forM hieFiles $ \hiePath -> do
        (hieFileResult, _newCache) <- readHieFile nameCache hiePath
        pure $ hie_file_result hieFileResult

createNameCache :: IO NameCache
createNameCache = do
    uniqSupply <- mkSplitUniqSupply 'z'
    pure $ initNameCache uniqSupply []

-- | Get the number of lines of code in the file by analising 'HieFile'.
countLinesOfCode :: HieFile -> Int
countLinesOfCode HieFile{..} = length $ BS8.lines hie_hs_src
