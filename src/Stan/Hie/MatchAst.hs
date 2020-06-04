{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Some 'Stan.Inspection.Inspection's require to know about AST and some
mechanism to match parts of syntax tree to the given
'PatternAst'. This information on AST expressions is taken from @HIE
files@ in a more suitable view.

This module contains an implementation of the process of retrieval of AST
information from @HIE@ files.
-}

module Stan.Hie.MatchAst
    ( hieMatchPatternAst
    ) where

import HieTypes (HieAST (..), HieFile (..), Identifier, IdentifierDetails, NodeInfo (..), TypeIndex)
import SrcLoc (RealSrcSpan, srcSpanEndCol, srcSpanStartCol, srcSpanStartLine)

import Stan.Core.List (checkWith)
import Stan.Hie.MatchType (hieMatchPatternType)
import Stan.NameMeta (NameMeta, hieMatchNameMeta)
import Stan.Pattern.Ast (PatternAst (..))
import Stan.Pattern.Type (PatternType)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Relude.Unsafe as Unsafe


{- | Matching function that matches current AST node with a given
pattern.
-}
hieMatchPatternAst
    :: HieFile  -- ^ HIE file
    -> HieAST TypeIndex  -- ^ Current AST node to match
    -> PatternAst  -- ^ Pattern to match against
    -> Bool  -- ^ 'True' if pattern matches AST node
hieMatchPatternAst hie@HieFile{..} node@Node{..} = \case
    PatternAstAnything -> True
    PatternAstNeg p ->
        not (hieMatchPatternAst hie node p)
    PatternAstOr p1 p2 ->
           hieMatchPatternAst hie node p1
        || hieMatchPatternAst hie node p2
    PatternAstAnd p1 p2 ->
           hieMatchPatternAst hie node p1
        && hieMatchPatternAst hie node p2
    PatternAstConstant n ->
           Set.member ("HsOverLit", "HsExpr") (nodeAnnotations nodeInfo)
        && readMaybe (decodeUtf8 $ slice nodeSpan) == Just n
    PatternAstName nameMeta patType ->
        any (matchNameAndType nameMeta patType)
        $ Map.assocs
        $ nodeIdentifiers nodeInfo
    PatternAstNode tags ->
        tags `Set.isSubsetOf` nodeAnnotations nodeInfo
    PatternAstNodeExact tags patChildren ->
           tags `Set.isSubsetOf` nodeAnnotations nodeInfo
        && checkWith (hieMatchPatternAst hie) nodeChildren patChildren
  where
    -- take sub-bytestring from src according to a given span
    -- TODO: current works only with single-line spans
    slice :: RealSrcSpan -> ByteString
    slice span =
        BS.take (srcSpanEndCol span - srcSpanStartCol span)
        $ BS.drop (srcSpanStartCol span - 1)
        $ Unsafe.at (srcSpanStartLine span - 1)
        $ BS8.lines hie_hs_src

    matchNameAndType
        :: NameMeta
        -> PatternType
        -> (Identifier, IdentifierDetails TypeIndex)
        -> Bool
    matchNameAndType nameMeta patType ids =
        hieMatchNameMeta nameMeta ids
        && case nodeType nodeInfo of
            []    -> False
            t : _ -> hieMatchPatternType hie_types patType t
