{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

HIE AST visitor for single-pass traversal.
-}

module Stan.Analysis.Visitor
    ( VisitorState (..)
    , getFinalObservations
    , addObservation
    , addObservations
    , addFixity
    , addOpDecl
    , addSpecializePragma
    , addFunToSpecialize

    , Visitor (..)
    , visitAst
    ) where

import Relude.Extra.Lens (Lens', lens, over)

import Stan.Ghc.Compat (RealSrcSpan)
import Stan.Hie.Compat (HieAST (..), HieASTs (..), HieFile (..), TypeIndex)
import Stan.Inspection (Inspection, inspectionId)
import Stan.Inspection.Performance (stan0401)
import Stan.Inspection.Style (stan0301)
import Stan.Observation (Observation, Observations, mkObservation)

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Slist as S


{- | State for the 'Visitor' object that stores all values during a
single HIE AST traversal.
-}
data VisitorState = VisitorState
    { visitorStateObservations      :: !Observations

      -- Operators for STAN-0301
    , visitorStateFixities          :: !(HashMap Text ())
    , visitorStateOpDecls           :: !(HashMap Text RealSrcSpan)

      -- Operators for STAN-0401
    , visitorStateSpecializePragmas :: !(HashMap Text ())
    , visitorStateFunsToSpecialize  :: !(HashMap Text RealSrcSpan)
    }

-- | Initial empty state.
initialVisitorState :: VisitorState
initialVisitorState = VisitorState
    { visitorStateObservations      = mempty
    , visitorStateFixities          = mempty
    , visitorStateOpDecls           = mempty
    , visitorStateSpecializePragmas = mempty
    , visitorStateFunsToSpecialize  = mempty
    }

{- | Transform 'VisitorState' to the final list of observations for
the given 'HieFile'. 'VisitorState' stores not only ready
'Observations' but also additional metadata collected during tree
traversal, so this metadata is converted to 'Observations' for the
corresponding 'Inspection's.
-}
finaliseState :: HieFile -> VisitorState -> Observations
finaliseState hie VisitorState{..} =
    -- STAN-0301: missing fixity declaration
    -- detected by finding a difference between two sets:
    -- 1. Top-level defined operators
    -- 2. Fixity declarations for operators in module
    let stan0301inss = evalInspections stan0301 visitorStateOpDecls visitorStateFixities
        stan0401inss = evalInspections stan0401 visitorStateFunsToSpecialize visitorStateSpecializePragmas
    -- combine final observations
    in visitorStateObservations
    <> stan0301inss
    <> stan0401inss
  where
    evalInspections :: Inspection -> HashMap Text RealSrcSpan -> HashMap Text () -> Observations
    evalInspections ins mapOfAll mapExclude = mkObservation (inspectionId ins) hie <$>
        S.slist (toList $ HM.difference mapOfAll mapExclude)

-- | Get sized list of all 'Observations' from the given HIE file
-- using the created 'Visitor'.
getFinalObservations :: HieFile -> Visitor -> Observations
getFinalObservations hie visitor =
    let visitAction = traverse_ (visitAst visitor) allHieAsts
        resultState = execState visitAction initialVisitorState
    in finaliseState hie resultState
  where
    allHieAsts :: [HieAST TypeIndex]
    allHieAsts = Map.elems $ getAsts $ hie_asts hie

observationsL :: Lens' VisitorState Observations
observationsL = lens
    visitorStateObservations
    (\vstate new -> vstate { visitorStateObservations = new })

fixitiesL :: Lens' VisitorState (HashMap Text ())
fixitiesL = lens
    visitorStateFixities
    (\vstate new -> vstate { visitorStateFixities = new })

opDeclsL :: Lens' VisitorState (HashMap Text RealSrcSpan)
opDeclsL = lens
    visitorStateOpDecls
    (\vstate new -> vstate { visitorStateOpDecls = new })

specializePragmasL :: Lens' VisitorState (HashMap Text ())
specializePragmasL = lens
    visitorStateSpecializePragmas
    (\vstate new -> vstate { visitorStateSpecializePragmas = new })

funsToSpecializeL :: Lens' VisitorState (HashMap Text RealSrcSpan)
funsToSpecializeL = lens
    visitorStateFunsToSpecialize
    (\vstate new -> vstate { visitorStateFunsToSpecialize = new })

-- | Add single 'Observation' to the existing 'VisitorState'.
addObservation :: Observation -> State VisitorState ()
addObservation obs = modify' $ over observationsL (S.one obs <>)

-- | Add 'Observations' to the existing 'VisitorState'.
addObservations :: Observations -> State VisitorState ()
addObservations obss
    | null obss = pass
    | otherwise = modify' $ over observationsL (obss <>)

-- | Add single operator infix declaration.
addFixity :: Text -> State VisitorState ()
addFixity fixity = modify' $ over fixitiesL (HM.insert fixity ())

-- | Add single operator top-level defintion with its position.
addOpDecl :: Text -> RealSrcSpan -> State VisitorState ()
addOpDecl opDecl srcSpan = modify' $ over opDeclsL (HM.insert opDecl srcSpan)

-- | Add single specialize pragma declaration declaration.
addSpecializePragma :: Text -> State VisitorState ()
addSpecializePragma pragma = modify' $ over specializePragmasL (HM.insert pragma ())

-- | Add single function that could be specialized top-level defintion with its position.
addFunToSpecialize :: Text -> RealSrcSpan -> State VisitorState ()
addFunToSpecialize fun srcSpan = modify' $ over funsToSpecializeL (HM.insert fun srcSpan)

-- | Object that implements the /Visitor pattern/.
newtype Visitor = Visitor
    { unVisitor :: HieAST TypeIndex -> State VisitorState ()
    }

-- | Traverse HIE AST starting from a given node using 'Visitor'.
visitAst :: Visitor -> HieAST TypeIndex -> State VisitorState ()
visitAst (Visitor visit) = go
  where
    go :: HieAST TypeIndex -> State VisitorState ()
    go node@Node{..} = do
        visit node
        traverse_ go nodeChildren
