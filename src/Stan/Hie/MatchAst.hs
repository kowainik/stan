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

import Data.Char (toLower)
import Prelude hiding (span)
import Stan.Core.List (checkWith)
import Stan.Ghc.Compat (nameOccName, occNameString)
import Stan.Hie (slice)
import Stan.Hie.Compat (ContextInfo (..), DeclType, HieAST (..), HieFile (..), Identifier,
                        IdentifierDetails (..), NodeInfo (..), TypeIndex, nodeInfo,
                        eqDeclType, NodeAnnotation, toNodeAnnotation)
import Stan.Hie.MatchType (hieMatchPatternType)
import Stan.NameMeta (NameMeta, hieMatchNameMeta)
import Stan.Pattern.Ast (Literal (..), PatternAst (..), literalAnns)
import Stan.Pattern.Type (PatternType)

import qualified Data.ByteString as BS
import qualified Data.List as Str
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


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
    PatternAstConstant lit ->
           Set.member literalAnns (Set.map toNodeAnnotation (nodeAnnotations nodeInfo'))
        && ( let span = slice nodeSpan hie_hs_src in case lit of
                ExactNum n   -> (span >>= readMaybe . decodeUtf8) == Just n
                ExactStr s   -> span == Just s
                PrefixStr s  -> maybe False (s `BS.isPrefixOf`) span
                ContainStr s -> maybe False (s `BS.isInfixOf`) span
                AnyLiteral   -> True
           )
    PatternAstName nameMeta patType ->
        any (matchNameAndType nameMeta patType)
        $ Map.assocs
        $ nodeIdentifiers nodeInfo'
    PatternAstNode tags ->
        matchAnnotations tags nodeInfo'
    PatternAstNodeExact tags patChildren ->
           matchAnnotations tags nodeInfo'
        && checkWith (hieMatchPatternAst hie) nodeChildren patChildren
    PatternAstVarName varName -> isJust $ find
        (\case
            Right x -> varName `Str.isInfixOf` map toLower (occNameString $ nameOccName x)
            Left _ -> False
        )
        $ Map.keys $ nodeIdentifiers nodeInfo'
    PatternAstIdentifierDetailsDecl declType -> any (any (isDecl declType) . identInfo) $
        Map.elems $ nodeIdentifiers nodeInfo'
  where
    matchAnnotations :: Set NodeAnnotation -> NodeInfo TypeIndex -> Bool
    matchAnnotations tags NodeInfo{..} =
      tags `Set.isSubsetOf` Set.map toNodeAnnotation nodeAnnotations

    nodeInfo' = Stan.Hie.Compat.nodeInfo node

    matchNameAndType
        :: NameMeta
        -> PatternType
        -> (Identifier, IdentifierDetails TypeIndex)
        -> Bool
    matchNameAndType nameMeta patType ids =
        hieMatchNameMeta nameMeta ids
        && case nodeType nodeInfo' of
            []    -> False
            t : _ -> hieMatchPatternType hie_types patType t

    isDecl :: DeclType -> ContextInfo -> Bool
    isDecl myDeclType (Decl curDeclType _) = myDeclType `eqDeclType` curDeclType
    isDecl _declType _otherContext         = False
