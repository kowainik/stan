{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Static analysis checks for infinite functions.
-}

module Stan.Analysis.Infinite
    ( stan0101Analyser
    , stan0102Analyser
    , stan0103Analyser

      -- * All infinite 'Analyser's
    , infiniteAnalysers
    ) where

import Stan.Analysis.Analyser (Analyser (..), mkNameMetaAnalyser)

import qualified Stan.Inspection.Infinite as Infinite


{- | Check for occurrences of the inifinite 'GHC.List.reverse' function.
@STAN-0101@.
-}
stan0101Analyser :: Analyser
stan0101Analyser = mkNameMetaAnalyser Infinite.stan0101 Infinite.stan0101Meta

{- | Check for occurrences of the inifinite 'GHC.List.isSuffixOf' function.
@STAN-0102@.
-}
stan0102Analyser :: Analyser
stan0102Analyser = mkNameMetaAnalyser Infinite.stan0102 Infinite.stan0102Meta

{- | Check for occurrences of the inifinite 'Data.OldList.genericLength' function.
@STAN-0103@.
-}
stan0103Analyser :: Analyser
stan0103Analyser = mkNameMetaAnalyser Infinite.stan0103 Infinite.stan0103Meta

-- | List of all infinite 'Analyser's.
infiniteAnalysers :: [Analyser]
infiniteAnalysers =
    [ stan0101Analyser
    , stan0102Analyser
    , stan0103Analyser
    ]
