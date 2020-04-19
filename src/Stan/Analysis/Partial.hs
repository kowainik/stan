{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Static analysis checks for partial functions.
-}

module Stan.Analysis.Partial
    ( stan0001Analyser
    , stan0002Analyser
    , stan0003Analyser
    , stan0004Analyser

      -- * All partial 'Analyser's
    , partialAnalysers
    ) where

import Stan.Analysis.Analyser (Analyser (..), mkNameMetaAnalyser)

import qualified Stan.Inspection.Partial as Partial


{- | Check for occurrences of the partial 'GHC.List.head' function.
@STAN-0001@.
-}
stan0001Analyser :: Analyser
stan0001Analyser = mkNameMetaAnalyser Partial.stan0001 Partial.stan0001Meta

{- | Check for occurrences of the partial 'GHC.List.tail' function.
@STAN-0002@.
-}
stan0002Analyser :: Analyser
stan0002Analyser = mkNameMetaAnalyser Partial.stan0002 Partial.stan0002Meta

{- | Check for occurrences of the partial 'GHC.List.init' function.
@STAN-0003@.
-}
stan0003Analyser :: Analyser
stan0003Analyser = mkNameMetaAnalyser Partial.stan0003 Partial.stan0003Meta

{- | Check for occurrences of the partial 'GHC.List.last' function.
@STAN-0004@.
-}
stan0004Analyser :: Analyser
stan0004Analyser = mkNameMetaAnalyser Partial.stan0004 Partial.stan0004Meta

-- | List of all partial 'Analyser's.
partialAnalysers :: [Analyser]
partialAnalysers =
    [ stan0001Analyser
    , stan0002Analyser
    , stan0003Analyser
    , stan0004Analyser
    ]
