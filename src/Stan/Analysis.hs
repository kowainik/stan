-- | In this module all analisys takes place.

module Stan.Analysis
       ( stan
       ) where

import HsDecls (HsGroup)
import HsExtension (GhcRn)

import Stan.Partial (findPartials)
import Stan.Warning (Warning)

stan :: HsGroup GhcRn -> [Warning]
stan = findPartials
