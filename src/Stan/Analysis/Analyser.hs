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

import Extensions (ExtensionsResult)
import GHC.LanguageExtensions.Type (Extension (Strict, StrictData))
import HieTypes (HieAST (..), HieASTs (..), HieFile (..), Identifier, NodeInfo (..), TypeIndex)
import Name (nameOccName)
import OccName (isSymOcc, occNameString)
import Slist (Slist, slist)
import SrcLoc (RealSrcSpan)

import Stan.Core.Id (Id)
import Stan.FileInfo (isExtensionDisabled)
import Stan.Hie.MatchAst (hieMatchPatternAst)
import Stan.Inspection (Inspection (..), InspectionAnalysis (..))
import Stan.Observation (Observations, mkObservation)
import Stan.Pattern.Ast (PatternAst, constructor, dataDecl, fixity, lazyField, tuple, typeSig)

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Slist as S


{- | Create analysing function for 'Inspection' by pattern-matching
over 'InspectionAnalysis'.
-}
analysisByInspection
    :: ExtensionsResult
    -> Inspection
    -> HieFile
    -> Observations
analysisByInspection exts Inspection{..} = case inspectionAnalysis of
    FindAst patAst -> analyseAst inspectionId patAst
    Infix -> analyseInfix inspectionId
    LazyField -> memptyIfFalse
        (isExtensionDisabled StrictData exts && isExtensionDisabled Strict exts)
        (analyseLazyFields inspectionId)
    BigTuples -> analyseBigTuples inspectionId

{- | Check for occurrences of the specified function given via 'NameMeta'.
-}
analyseAst
    :: Id Inspection
    -> PatternAst
    -> HieFile
    -> Observations
analyseAst insId patAst hie =
    mkObservation insId hie <$> analyseAstWith (createMatch patAst hie) hie

{- | Check for big tuples (size >= 4) in the following places:

* Type signatures: foo :: (Int, Int, Int, Int)
* Literals: (True, 0, [], Nothing)
-}
analyseBigTuples
    :: Id Inspection
    -> HieFile
    -> Observations
analyseBigTuples insId hie =
    S.map (mkObservation insId hie . nodeSpan)
    $ S.filter isBigTuple
    $ analyseAstWith (createMatchAst tuple hie) hie
  where
    isBigTuple :: HieAST TypeIndex -> Bool
    isBigTuple Node{..} = case nodeChildren of
        _:_:_:_:_ -> True
        _         -> False

{- | Check for occurrences lazy fields in all constructors. Ignores
@newtype@s. Currently HIE Ast doesn't have information whether the
data type is @newtype@ or not. So the algorithm ignores all data types
with a single constructor and single field inside that constructor.
-}
analyseLazyFields
    :: Id Inspection
    -> HieFile
    -> Observations
analyseLazyFields insId hie =
    mkObservation insId hie <$> analyseAstWith matchLazyField hie
  where
    matchLazyField :: HieAST TypeIndex -> Slist RealSrcSpan
    matchLazyField node = memptyIfFalse
        -- return empty list if it's not a data type
        (hieMatchPatternAst hie node dataDecl)
        -- get list of all constructors
        $ let constructors = filter
                (\n -> hieMatchPatternAst hie n constructor)
                (nodeChildren node)
          in case constructors of
              -- no constructors = not observations
              []  -> mempty
              -- single constructor
              [c] -> S.concatMap matchField $ extractFields False c
              -- multiple constructors = analyse everything
              cs  -> S.concatMap (S.concatMap matchField . extractFields True) cs

    -- Extract fields as AST nodes. Return empty list if only one field
    -- (as a workaround for the @newtype@ problem)
    --
    -- record constructors have 2 children:
    --   1. Constructor name.
    --   2. Dummy child with all fields as childrens
    -- plain constructors have constructor name and children in the same list
    extractFields :: Bool -> HieAST TypeIndex -> [HieAST TypeIndex]
    extractFields hasManyCtors ctor = case drop 1 $ nodeChildren ctor of
        [] -> []  -- no fields
        [n] ->  -- single field, maybe dummy record node
            if isDummyRecordNode n
            then case nodeChildren n of
                []      -> []
                [field] -> [field | hasManyCtors]
                fields  -> fields
            else [n | hasManyCtors]
        fields -> fields  -- plain constructor
      where
        -- simple check for the dummy AST node
        isDummyRecordNode :: HieAST TypeIndex -> Bool
        isDummyRecordNode = Set.null . nodeAnnotations . nodeInfo

    -- matches record fields non-recursively
    matchField :: HieAST TypeIndex -> Slist RealSrcSpan
    matchField = createMatch lazyField hie

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
        operatorsWithoutFixity = HM.difference topOperators fixities
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
    = Fixity !Text
    -- | Operator name with its position to display later
    | Operator !Text !RealSrcSpan

{- | Partition a foldable of operator declarations into two maps:

1. Fixity declarations (mapped to @()@).
2. Top-level operator names (mapped to their source positions.

'Map' is used to be able to use the nice @merge@ function.
-}
partitionDecls
    :: Foldable f
    => f OperatorDecl
    -> (HashMap Text (), HashMap Text RealSrcSpan)
partitionDecls = foldl' insertDecl mempty
  where
    insertDecl
        :: (HashMap Text (), HashMap Text RealSrcSpan)
        -> OperatorDecl
        -> (HashMap Text (), HashMap Text RealSrcSpan)
    insertDecl (!fixities, !topOperators) = \case
        Fixity name -> (HM.insert name () fixities, topOperators)
        Operator name srcSpan -> (fixities, HM.insert name srcSpan topOperators)

{- | Analyses the whole AST starting from the very top.
-}
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
        S.concatMap (matchAstWith match)
        . Map.elems
        . getAsts

{- | Recursively match AST nodes starting from a given AST.
-}
matchAstWith
    :: forall a
    .  (HieAST TypeIndex -> Slist a)
    -- ^ Function to match AST node to some arbitrary type and return a
    -- sized list of matched elements
    -> HieAST TypeIndex
    -> Slist a
matchAstWith match = matchAst
  where
    matchAst :: HieAST TypeIndex -> Slist a
    matchAst node@Node{..} =
        match node <> S.concatMap matchAst nodeChildren

-- | Like 'createMatchAst' but returns source spans of AST nodes.
createMatch :: PatternAst -> HieFile -> (HieAST TypeIndex -> Slist RealSrcSpan)
createMatch patAst hie = fmap nodeSpan . createMatchAst patAst hie

{- | Create a non-recursive matching function for 'PatternAst' that
returns sized list of nodes that match this pattern.

* If the pattern matches 'Node', return it
* Otherwise return empty list
-}
createMatchAst
    :: PatternAst
    -> HieFile
    -> (HieAST TypeIndex -> Slist (HieAST TypeIndex))
createMatchAst patAst hie node =
    memptyIfFalse (hieMatchPatternAst hie node patAst) (S.one node)
