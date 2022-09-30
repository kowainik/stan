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
import Stan.Hie.Compat (HieAST (..), HieFile (..), HieFileResult (hie_file_result),
                        NodeInfo (..), readHieFileWithNameCache, nodeInfo,
                        toNodeAnnotation)
import Stan.Hie.Debug ()
import Stan.Pattern.Ast (literalAnns)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Set as Set


{- | Returns contents of all @.hie@ files recursively in the given
@hie@ directory.
-}
readHieFiles :: FilePath -> IO [HieFile]
readHieFiles hieDir = do
    unlessM (doesDirectoryExist hieDir) $ do
        errorMessage $ "Directory with HIE files doesn't exist: " <> toText hieDir
        infoMessage "Use the '--hiedir' CLI option to specify path to the directory with HIE files"
        exitFailure

    readHieFile <- readHieFileWithNameCache
    hieContent <- getDirRecursive hieDir
    let isHieFile f = (&&) (takeExtension f == ".hie") <$> doesFileExist f
    hiePaths <- filterM isHieFile hieContent

    when (null hiePaths) $ warningMessage $
        "The directory with HIE files doesn't contain any HIE files: " <> toText hieDir

    forM hiePaths $ \hiePath -> do
        hieFileResult <- readHieFile hiePath
        pure $ hie_file_result hieFileResult

-- | Get the number of lines of code in the file by analising 'HieFile'.
countLinesOfCode :: HieFile -> Int
countLinesOfCode HieFile{..} = length $ BS8.lines hie_hs_src

{- | Take sub-bytestring according to a given span.

When the given source is empty returns 'Nothing'.

TODO: currently works only with single-line spans
-}
slice :: RealSrcSpan -> ByteString -> Maybe ByteString
slice span =
    fmap
        ( BS.take (srcSpanEndCol span - srcSpanStartCol span)
        . BS.drop (srcSpanStartCol span - 1)
        )
    . flip (!!?) (srcSpanStartLine span - 1)
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
    eqNodes n1@(Node _ span1 children1) n2@(Node _ span2 children2) =
        eqInfo info1 info2 && checkWith eqNodes children1 children2
      where
        info1 = nodeInfo n1
        info2 = nodeInfo n2

        eqInfo :: NodeInfo a -> NodeInfo a -> Bool
        eqInfo (NodeInfo anns1 types1 ids1) (NodeInfo anns2 types2 ids2) =
            anns1 == anns2 && types1 == types2 && ids1 == ids2 &&
            if Set.member literalAnns (Set.map toNodeAnnotation anns1)
            then slice span1 hie_hs_src == slice span2 hie_hs_src
            else True
