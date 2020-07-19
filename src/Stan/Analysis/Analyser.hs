{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Analysing functions by 'InspectionAnalysis' for the corresponding
'Inspection'.
-}

module Stan.Analysis.Analyser
    ( analyseAst
    ) where

import Extensions (ExtensionsResult)
import GHC.LanguageExtensions.Type (Extension (Strict, StrictData))
import Slist (Slist)

import Stan.Analysis.Visitor (Visitor (..), VisitorState (..), addFixity, addObservation,
                              addObservations, addOpDecl, getFinalObservations)
import Stan.Core.Id (Id)
import Stan.Core.List (nonRepeatingPairs)
import Stan.FileInfo (isExtensionDisabled)
import Stan.Ghc.Compat (RealSrcSpan, isSymOcc, nameOccName, occNameString)
import Stan.Hie (eqAst)
import Stan.Hie.Compat (HieAST (..), HieFile (..), Identifier, NodeInfo (..), TypeIndex)
import Stan.Hie.MatchAst (hieMatchPatternAst)
import Stan.Inspection (Inspection (..), InspectionAnalysis (..))
import Stan.NameMeta (NameMeta, ghcPrimNameFrom)
import Stan.Observation (Observations, mkObservation)
import Stan.Pattern.Ast (Literal (..), PatternAst (..), anyNamesToPatternAst, case', constructor,
                         constructorNameIdentifier, dataDecl, fixity, fun, guardBranch, lambdaCase,
                         lazyField, literalPat, opApp, patternMatchArrow, patternMatchBranch,
                         patternMatch_, rhs, tuple, typeSig)
import Stan.Pattern.Edsl (PatternBool (..))

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Slist as S

{- | Analyses the whole AST starting from the very top.
-}
analyseAst
    :: HieFile
    -> ExtensionsResult
    -> [Inspection]
    -> Observations
analyseAst hie exts = getFinalObservations hie . createVisitor hie exts

{- | Create a sinble 'Visitor' value from a list of 'Inspection's and
additional read-only context. This 'Visitor' can be used to traverse
HIE AST in a single pass.
-}
createVisitor
    :: HieFile
    -> ExtensionsResult
    -> [Inspection]
    -> Visitor
createVisitor hie exts inspections = Visitor $ \node ->
    forM_ inspections $ \Inspection{..} -> case inspectionAnalysis of
        FindAst patAst -> matchAst inspectionId patAst hie node
        Infix -> analyseInfix hie node
        LazyField -> when
            (isExtensionDisabled StrictData exts && isExtensionDisabled Strict exts)
            (analyseLazyFields inspectionId hie node)
        BigTuples -> analyseBigTuples inspectionId hie node
        PatternMatchOn_ -> analysePatternMatch_ inspectionId hie node
        UseCompare -> analyseCompare inspectionId hie node

{- | Check for big tuples (size >= 4) in the following places:

* Type signatures: foo :: (Int, Int, Int, Int)
* Literals: (True, 0, [], Nothing)
-}
analyseBigTuples
    :: Id Inspection
    -> HieFile
    -> HieAST TypeIndex
    -> State VisitorState ()
analyseBigTuples insId = matchAstWith isBigTuple insId tuple
  where
    isBigTuple :: HieAST TypeIndex -> Bool
    isBigTuple Node{..} = case nodeChildren of
        _:_:_:_:_  -> True
        _lessThan4 -> False

{- | Find usages of multiple comparison operators and suggest using
'compare'. Currently, handles the following cases:

* Guards

The algorithm is to find all guards, filter them by usage of
comparison operators and find matches.
-}
analyseCompare
    :: Id Inspection
    -> HieFile
    -> HieAST TypeIndex
    -> State VisitorState ()
analyseCompare insId hie curNode =
    addObservations $ mkObservation insId hie <$> matchComparisonGuards curNode
  where
    matchComparisonGuards :: HieAST TypeIndex -> Slist RealSrcSpan
    matchComparisonGuards node = memptyIfFalse
        (hieMatchPatternAst hie node fun)
        $ let guards = mapMaybe extractComparisonGuard (nodeChildren node)
          in memptyIfFalse (hasManyCompares guards) (S.one $ nodeSpan node)

    {- Extract left argument, name of a comparison operator and right
    argument from a guard.
    -}
    extractComparisonGuard
        :: HieAST TypeIndex
        -> Maybe (HieAST TypeIndex, HieAST TypeIndex)
    extractComparisonGuard node = do
        -- guard starts with GRHS annotation
        guard $ hieMatchPatternAst hie node rhs
        -- guard predicate is a first son
        stmt:_ <- Just $ nodeChildren node
        -- check if it's a guard
        guard $ hieMatchPatternAst hie stmt guardBranch
        -- check if it's an operator
        guard $ hieMatchPatternAst hie stmt $ opApp (?) opsPat (?)
        -- extract comparison
        x:_opAst:y:_ <- Just $ nodeChildren stmt
        pure (x, y)

    -- pattern for any comparison operator
    opsPat :: PatternAst
    opsPat = anyNamesToPatternAst $ le :| [leq, eq, ge, geq]

    le, leq, eq, ge, geq :: NameMeta
    le  = opName "<"
    leq = opName "<="
    eq  = opName "=="
    ge  = opName ">"
    geq = opName ">="

    opName :: Text -> NameMeta
    opName = (`ghcPrimNameFrom` "GHC.Classes")

    -- return True if any two pairs perform comparison of similar things
    hasManyCompares :: [(HieAST TypeIndex, HieAST TypeIndex)] -> Bool
    hasManyCompares = any (uncurry matchingComparions) . nonRepeatingPairs

    matchingComparions
        :: (HieAST TypeIndex, HieAST TypeIndex)
        -> (HieAST TypeIndex, HieAST TypeIndex)
        -> Bool
    matchingComparions (a, b) (x, y) =
        (eqAst hie a x && eqAst hie b y) || (eqAst hie a y && eqAst hie b x)


{- | Check for occurrences lazy fields in all constructors. Ignores
@newtype@s. Currently HIE Ast doesn't have information whether the
data type is @newtype@ or not. So the algorithm ignores all data types
with a single constructor and single field inside that constructor.
-}
analyseLazyFields
    :: Id Inspection
    -> HieFile
    -> HieAST TypeIndex
    -> State VisitorState ()
analyseLazyFields insId hie curNode =
    addObservations $ mkObservation insId hie <$> matchLazyField curNode
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
    -- record constructors have the following children:
    --   1. One or many constraints (e.g. forall a . Num a =>)
    --   2. Constructor name.
    --   3. Dummy child with all fields as childrens
    -- plain constructors have constructor name and children in the same list
    extractFields :: Bool -> HieAST TypeIndex -> [HieAST TypeIndex]
    extractFields hasManyCtors ctor = case drop 1 $ dropWhile isConstraint $ nodeChildren ctor of
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

        -- Not the constructor identifier
        isConstraint :: HieAST TypeIndex -> Bool
        isConstraint n = not $ hieMatchPatternAst hie n constructorNameIdentifier

    -- matches record fields non-recursively
    matchField :: HieAST TypeIndex -> Slist RealSrcSpan
    matchField = createMatch lazyField hie

{- | Check for occurrences of pattern matching on @_@ for sum types (except
literals).
-}
analysePatternMatch_
    :: Id Inspection
    -> HieFile
    -> HieAST TypeIndex
    -> State VisitorState ()
analysePatternMatch_ insId hie curNode =
    addObservations $ mkObservation insId hie <$> matchPatternMatch curNode
  where
    matchPatternMatch :: HieAST TypeIndex -> Slist RealSrcSpan
    matchPatternMatch node = memptyIfFalse
        -- return empty list if it's not a case or lambda case
        (hieMatchPatternAst hie node $ lambdaCase ||| case')
        -- get list of all case branches
        $ case nodeChildren node of
              -- no branches = not observations
              []     -> mempty
              -- lambda case, first kid is pattern matching
              [pm]   -> analyseBranches pm
              -- case, first kid is @case exp of@, the second is pattern matching
              _:pm:_ -> analyseBranches pm

    {- Check the pattern matching child on some particular expressions.

    -}
    analyseBranches :: HieAST TypeIndex -> Slist RealSrcSpan
    analyseBranches pm = case nodeChildren pm of
        -- if there is no children = no observations
        [] -> mempty
        -- we need to check first and all other children separately
        -- see 'isFirstPatternMatchBranchOk' comment to understand the first
        -- child's rules.
        c:cs -> memptyIfFalse (isFirstPatternMatchBranchOk c) $
            {- if the first child satisfies rules of the first pattern matching
            branch, then we need to find the child with pattern matching on @_@.
            If there is no such expression = all is good.
            -}
            case find (\x -> hieMatchPatternAst hie x (patternMatch_ (?))) cs of
                Nothing -> mempty
                Just e  -> S.one (nodeSpan e)

    {- The first pattern matching branch should not:
    1. Be empty (makes no sense)
    2. Be a literal pattern matching (e.g. on 'Int's or 'String's)
    In all other cases we can continue our matching checks with other children.
    -}
    isFirstPatternMatchBranchOk :: HieAST TypeIndex -> Bool
    isFirstPatternMatchBranchOk c = hieMatchPatternAst hie c patternMatchBranch &&
        case takeWhile isNotMatchArrow $ nodeChildren c of
            []  -> False
            [x] -> hieMatchPatternAst hie x notLiteral
            _:_ -> True
      where
        isNotMatchArrow :: HieAST TypeIndex -> Bool
        isNotMatchArrow n = hieMatchPatternAst hie n $ neg $ patternMatchArrow (?)

    notLiteral :: PatternAst
    notLiteral = neg
        -- general literal expression
        ( PatternAstConstant AnyLiteral
        -- since GHC-8.10 expression for literal in pattern matching
        ||| literalPat
        )

{- | Analyse HIE AST to find all operators which lack explicit fixity
declaration.

The algorithm is the following:

1. Traverse AST and discover all top-level operators and @infix@
declarations in a single pass.
2. Compare two resulting sets to find out operators without @infix@
declaration.
-}
analyseInfix
    :: HieFile
    -> HieAST TypeIndex
    -> State VisitorState ()
analyseInfix hie curNode = do
    matchInfix curNode
    matchOperator curNode
  where
    -- adds to the state list of operator names defined in a single
    -- fixity declaration:
    -- infix 5 ***, +++, ???
    matchInfix :: HieAST TypeIndex -> State VisitorState ()
    matchInfix node@Node{..} = when
        (hieMatchPatternAst hie node fixity)
        (traverse_ addFixity $ concatMap nodeIds nodeChildren)

    -- add to state a singleton or empty list with the top-level
    -- operator definition:
    -- (+++) :: ...
    matchOperator :: HieAST TypeIndex -> State VisitorState ()
    matchOperator node@Node{..} = when
        (hieMatchPatternAst hie node typeSig)
        (whenJust
            -- do nothing when cannot extract name
            -- first child of a parent is a name of a function/operator
            (viaNonEmpty (extractOperatorName . head) nodeChildren)
            -- add each operator decl from a list (should be singleton list)
            (traverse_ (uncurry addOpDecl))
        )

    -- return AST node identifier names as a sized list of texts
    nodeIds :: HieAST TypeIndex -> [Text]
    nodeIds =
        concatMap fixityName
        . Map.keys
        . nodeIdentifiers
        . nodeInfo

    fixityName :: Identifier -> [Text]
    fixityName = \case
        Left _ -> []
        Right name -> [toText $ occNameString $ nameOccName name]

    extractOperatorName :: HieAST TypeIndex -> [(Text, RealSrcSpan)]
    extractOperatorName Node{..} =
        concatMap (topLevelOperatorName nodeSpan)
        $ Map.keys
        $ nodeIdentifiers nodeInfo

    topLevelOperatorName :: RealSrcSpan -> Identifier -> [(Text, RealSrcSpan)]
    topLevelOperatorName srcSpan = \case
        Left _ -> []
        Right name ->
            let occName = nameOccName name
            -- return empty list if identifier name is not operator name
            in [(toText $ occNameString occName, srcSpan) | isSymOcc occName]

-- | Returns source spans of matched AST nodes.
createMatch
    :: PatternAst
    -> HieFile
    -> HieAST TypeIndex
    -> Slist RealSrcSpan
createMatch patAst hie node =
    memptyIfFalse (hieMatchPatternAst hie node patAst) (S.one $ nodeSpan node)

{- | Specialized version of 'matchAstWith' where custom predicate
always returns 'True'.
-}
matchAst
    :: Id Inspection
    -> PatternAst
    -> HieFile
    -> HieAST TypeIndex  -- ^ Current node
    -> State VisitorState ()
matchAst = matchAstWith (const True)

{- | Add observation to the state if the given node matches the given
'PatternAst' exactly (non-recursively) and if the given custom
predicate returns 'True'..
-}
matchAstWith
    :: (HieAST TypeIndex -> Bool)  -- ^ Custom node check
    -> Id Inspection
    -> PatternAst
    -> HieFile
    -> HieAST TypeIndex  -- ^ Current node
    -> State VisitorState ()
matchAstWith check insId patAst hie node@Node{..} =
    when (hieMatchPatternAst hie node patAst && check node) $
        addObservation $ mkObservation insId hie nodeSpan
