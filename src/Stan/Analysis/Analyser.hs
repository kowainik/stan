{-# LANGUAGE BangPatterns #-}

{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Analysing functions by 'InspectionAnalysis' for the corresponding
'Inspection'.
-}

module Stan.Analysis.Analyser
    ( analysisByInspection
    ) where

import HieTypes (HieAST (..), HieASTs (..), HieFile (..), Identifier, IdentifierDetails (..),
                 NodeInfo (..), TypeIndex)
import Name (nameOccName)
import OccName (isSymOcc, occNameString)
import Slist (Slist, slist)
import SrcLoc (RealSrcSpan)

import Stan.Core.Id (Id)
import Stan.Hie.MatchAst (hieMatchPatternAst)
import Stan.Hie.MatchType (hieMatchPatternType)
import Stan.Inspection (Inspection (..), InspectionAnalysis (..))
import Stan.NameMeta (NameMeta, hieMatchNameMeta)
import Stan.Observation (Observations, mkObservation)
import Stan.Pattern.Ast (PatternAst, fixity, typeSig)
import Stan.Pattern.Type (PatternType)

import qualified Data.Map.Strict as Map
import qualified Slist as S


{- | Create analysing function for 'Inspection' by pattern-matching
over 'InspectionAnalysis'.
-}
analysisByInspection :: Inspection -> HieFile -> Observations
analysisByInspection Inspection{..} = case inspectionAnalysis of
    FindName nameMeta patType -> analyseNameMeta inspectionId nameMeta patType
    FindAst patAst            -> analyseAst inspectionId patAst
    Infix                     -> analyseInfix inspectionId

{- | Check for occurrences of the specified function given via 'NameMeta'.
-}
analyseNameMeta
  :: Id Inspection
  -> NameMeta
  -> PatternType
  -> HieFile
  -> Observations
analyseNameMeta insId nameMeta patType hie@HieFile{..} =
    mkObservation insId hie <$> findSpans hie_asts
  where
    findSpans :: HieASTs TypeIndex -> Slist RealSrcSpan
    findSpans =
        S.concatMap findInAst
        . Map.elems
        . getAsts

    findInAst :: HieAST TypeIndex -> Slist RealSrcSpan
    findInAst Node{..} =
        findInNode nodeSpan nodeInfo <> S.concatMap findInAst nodeChildren

    findInNode :: RealSrcSpan -> NodeInfo TypeIndex -> Slist RealSrcSpan
    findInNode srcSpan NodeInfo{..} = slist
        $ mapMaybe (findUsage nodeType srcSpan)
        $ Map.assocs nodeIdentifiers

    findUsage
        :: [TypeIndex]
        -> RealSrcSpan
        -> (Identifier, IdentifierDetails TypeIndex)
        -> Maybe RealSrcSpan
    findUsage typeIxs srcSpan hieId = do
        guard
            -- matches with the given nameMeta
            $ hieMatchNameMeta nameMeta hieId
            -- compatible with the given pattern
            && case typeIxs of
                []    -> False
                t : _ -> hieMatchPatternType hie_types patType t

        pure srcSpan

{- | Check for occurrences of the specified function given via 'NameMeta'.
-}
analyseAst
  :: Id Inspection
  -> PatternAst
  -> HieFile
  -> Observations
analyseAst insId patAst hie =
    mkObservation insId hie <$> analyseAstWith matchPattern hie
  where
    matchPattern :: HieAST TypeIndex -> Slist RealSrcSpan
    matchPattern node@Node{..} =
        memptyIfFalse (hieMatchPatternAst hie node patAst) (S.one nodeSpan)

{- | Analyse HIE AST to find all operators which lack explicit fixity
declaration.

The algorithm is the following:

1. Traverse AST and discover all top-level operators and @infix@
declarations in a single pass.
2. Compare two resulting sets to find out operators without @infix@
declaration.
-}
analyseInfix
    :: Id Inspection
    -> HieFile
    -> Observations
analyseInfix insId hie =
    let opDecls = analyseAstWith (matchInfix <> matchOperator) hie
        (fixities, topOperators) = partitionDecls opDecls
        operatorsWithoutFixity = Map.difference topOperators fixities
    in mkObservation insId hie <$> slist (toList operatorsWithoutFixity)
  where
    -- returns list of operator names defined in a single fixity declaration:
    -- infix 5 ***, +++, ???
    matchInfix :: HieAST TypeIndex -> Slist OperatorDecl
    matchInfix node@Node{..} = memptyIfFalse
        (hieMatchPatternAst hie node fixity)
        (S.concatMap nodeIds nodeChildren)

    -- singleton or empty list with the top-level operator definition
    matchOperator :: HieAST TypeIndex -> Slist OperatorDecl
    matchOperator node@Node{..} = memptyIfFalse
        (hieMatchPatternAst hie node typeSig)
        (maybeToMonoid $ viaNonEmpty (extractOperatorName . head) nodeChildren)
        -- first child of a parent is a name of a function/operator

    -- return AST node identifier names as a sized list of texts
    nodeIds :: HieAST TypeIndex -> Slist OperatorDecl
    nodeIds =
        S.concatMap identifierName
        . Map.keys
        . nodeIdentifiers
        . nodeInfo

    identifierName :: Identifier -> Slist OperatorDecl
    identifierName = \case
        Left _ -> mempty
        Right name -> S.one $ Fixity $ toText $ occNameString $ nameOccName name

    extractOperatorName :: HieAST TypeIndex -> Slist OperatorDecl
    extractOperatorName Node{..} =
        S.concatMap (topLevelOperatorName nodeSpan)
        $ Map.keys
        $ nodeIdentifiers nodeInfo

    topLevelOperatorName :: RealSrcSpan -> Identifier -> Slist OperatorDecl
    topLevelOperatorName srcSpan = \case
        Left _ -> mempty
        Right name ->
            let occName = nameOccName name
            in memptyIfFalse
                (isSymOcc occName)  -- check if operator
                (S.one $ Operator (toText $ occNameString occName) srcSpan)

-- | Either top-level operator or fixity declaration
data OperatorDecl
    = Fixity Text
    -- | Operator name with its position to display later
    | Operator Text RealSrcSpan

{- | Partition a foldable of operator declarations into two maps:

1. Fixity declarations (mapped to @()@).
2. Top-level operator names (mapped to their source positions.

'Map' is used to be able to use the nice @merge@ function.
-}
partitionDecls :: Foldable f => f OperatorDecl -> (Map Text (), Map Text RealSrcSpan)
partitionDecls = foldl' insertDecl mempty
  where
    insertDecl
        :: (Map Text (), Map Text RealSrcSpan)
        -> OperatorDecl
        -> (Map Text (), Map Text RealSrcSpan)
    insertDecl (!fixities, !topOperators) = \case
        Fixity name -> (Map.insert name () fixities, topOperators)
        Operator name srcSpan -> (fixities, Map.insert name srcSpan topOperators)

analyseAstWith
  :: forall a
  .  (HieAST TypeIndex -> Slist a)
  -- ^ Function to match AST node to some arbitrary type and return a
  -- sized list of matched elements
  -> HieFile
  -> Slist a
analyseAstWith match = findNodes . hie_asts
  where
    findNodes :: HieASTs TypeIndex -> Slist a
    findNodes =
        S.concatMap matchAst
        . Map.elems
        . getAsts

    matchAst :: HieAST TypeIndex -> Slist a
    matchAst node@Node{..} =
        match node <> S.concatMap matchAst nodeChildren
