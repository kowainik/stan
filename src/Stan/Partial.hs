{- | This module helps to diagnose usage of the partial function
in the code. Ex.: 'head', 'tail', 'last', 'fromJust', etc.
-}

module Stan.Partial
       ( findPartials
       ) where

import Control.Monad (guard)
import Data.Bifunctor (bimap)
import Data.Generics.Schemes (listify)
import Data.Maybe (mapMaybe)
import HsDecls (HsGroup)
import HsExtension (GhcRn)
import HsInstances ()
import Module (ModuleName, mkModuleName, moduleName)
import Name (Name, nameModule, nameOccName, occNameString)
import OccName (OccName, mkVarOcc)
import SrcLoc (GenLocated (L), Located, RealSrcSpan, SrcSpan (..))

import Stan.Warning (Warning (..))

-- | Returns the list of the 'Partial' warnings if it finds any.
findPartials :: HsGroup GhcRn -> [Warning]
findPartials = mapMaybe toWarning . lNames
  where
    lNames :: HsGroup GhcRn -> [Located Name]
    lNames = listify (\(_ :: Located Name) -> True)

    toWarning :: Located Name -> Maybe Warning
    toWarning (L l name) =
        let modName = moduleName $ nameModule name
            occName = nameOccName name
        in guard (ifPartial modName occName && isRealSrcSpan l)
               *> Just (Partial (occNameString occName) $ getReal l)
         -- Just $ Partial (moduleNameString (moduleName $ nameModule name) ++ occNameString (nameOccName name))

-- | The list of the partial functions and where they comes from.
partialFuns :: [(ModuleName, OccName)]
partialFuns = map (bimap mkModuleName mkVarOcc)
    [ ("GHC.List",   "head")
    , ("GHC.List",   "tail")
    , ("GHC.List",   "init")
    , ("GHC.List",   "last")
    , ("GHC.List",   "!!")
    , ("Data.Maybe", "fromJust")
    ]

-- | Checks if the current 'Name' is in the list of the partial functions.
ifPartial :: ModuleName -> OccName -> Bool
ifPartial modName occName = any checkFun partialFuns
  where
    checkFun :: (ModuleName, OccName) -> Bool
    checkFun (pModName, pOccName) = pModName == modName
                                 && pOccName == occName

isRealSrcSpan :: SrcSpan -> Bool
isRealSrcSpan (RealSrcSpan _) = True
isRealSrcSpan _               = False

getReal :: SrcSpan -> RealSrcSpan
getReal (RealSrcSpan src) = src
getReal _                 = error "Impossible happened"
