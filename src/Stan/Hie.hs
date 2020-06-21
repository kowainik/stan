{- HLINT ignore "Redundant if" -}

{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Functions to work with @hie@ specific parts.
-}

module Stan.Hie
    ( readHieFiles
    , countLinesOfCode
    , eqAst
    , slice
    ) where

import Colourista (errorMessage, infoMessage, warningMessage)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Directory.Recursive (getDirRecursive)
import System.FilePath (takeExtension)

import Stan.Core.List (checkWith)
import Stan.Ghc.Compat (RealSrcSpan, srcSpanEndCol, srcSpanStartCol, srcSpanStartLine)
import Stan.Hie.Compat (HieAST (..), HieFile (..), HieFileResult (hie_file_result), NameCache,
                        NodeInfo (..), initNameCache, mkSplitUniqSupply, readHieFile)
import Stan.Hie.Debug ()
import Stan.Pattern.Ast (literalAnns)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Set as Set
import qualified Relude.Unsafe as Unsafe


{- | Returns contents of all @.hie@ files recursively in the given
@hie@ directory.
-}
readHieFiles :: FilePath -> IO [HieFile]
readHieFiles hieDir = do
    unlessM (doesDirectoryExist hieDir) $ do
        errorMessage $ "Directory with HIE files doesn't exist: " <> toText hieDir
        infoMessage "Use the '--hiedir' CLI option to specify path to the directory with HIE files"
        exitFailure

    nameCache <- createNameCache
    hieContent <- getDirRecursive hieDir
    let isHieFile f = (&&) (takeExtension f == ".hie") <$> doesFileExist f
    hiePaths <- filterM isHieFile hieContent

    when (null hiePaths) $ warningMessage $
        "The directory with HIE files doesn't contain any HIE files: " <> toText hieDir

    forM hiePaths $ \hiePath -> do
        (hieFileResult, _newCache) <- readHieFile nameCache hiePath
        pure $ hie_file_result hieFileResult

createNameCache :: IO NameCache
createNameCache = do
    uniqSupply <- mkSplitUniqSupply 'z'
    pure $ initNameCache uniqSupply []

-- | Get the number of lines of code in the file by analising 'HieFile'.
countLinesOfCode :: HieFile -> Int
countLinesOfCode HieFile{..} = length $ BS8.lines hie_hs_src

{- | Take sub-bytestring according to a given span.

TODO: currently works only with single-line spans
-}
slice :: RealSrcSpan -> ByteString -> ByteString
slice span =
    BS.take (srcSpanEndCol span - srcSpanStartCol span)
    . BS.drop (srcSpanStartCol span - 1)
    . Unsafe.at (srcSpanStartLine span - 1)
    . BS8.lines

{- | Compare two AST nodes on equality. This is a more relaxed version
of the 'Eq' instance for 'HieAST' because it doesn't compare source
locations. This function is useful if you want to check whether two
AST nodes represent the same AST.

This function needs to take the original 'HieFile' because constants
are not stored in 'HieAST' and to compare constants we need to compare
parts of source code.
-}
eqAst :: forall a . Eq a => HieFile -> HieAST a -> HieAST a -> Bool
eqAst HieFile{..} = eqNodes
  where
    eqNodes :: HieAST a -> HieAST a -> Bool
    eqNodes (Node info1 span1 children1) (Node info2 span2 children2) =
        eqInfo info1 info2 && checkWith eqNodes children1 children2
      where
        eqInfo :: NodeInfo a -> NodeInfo a -> Bool
        eqInfo (NodeInfo anns1 types1 ids1) (NodeInfo anns2 types2 ids2) =
            anns1 == anns2 && types1 == types2 && ids1 == ids2 &&
            if Set.member literalAnns anns1
            then slice span1 hie_hs_src == slice span2 hie_hs_src
            else True
