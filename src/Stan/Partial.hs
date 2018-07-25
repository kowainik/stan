{-# LANGUAGE ScopedTypeVariables #-}

{- | This module helps to diagnose usage of the partial function
in the code. Ex.: 'head', 'tail', 'last', 'fromJust', etc.
-}

module Stan.Partial
       ( findPartials
       ) where

import Data.Generics.Schemes (listify)
import Data.Maybe (mapMaybe)
import HsDecls (HsGroup)
import HsExtension (GhcRn)
import HsInstances ()
import Module (ModuleName, mkModuleName, moduleName)
import Name (Name, nameModule, nameOccName, occNameString)
import OccName (OccName, mkVarOcc)
import SrcLoc (GenLocated (L), Located)

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
        in
        if checkName modName occName
            then Just $ Partial (occNameString occName) l
            else Nothing
         -- Just $ Partial (moduleNameString (moduleName $ nameModule name) ++ occNameString (nameOccName name))

-- | The list of the partial functions and where they comes from.
partialFuns :: [(ModuleName, OccName)]
partialFuns = [ (mkModuleName "GHC.List",   mkVarOcc "head")
              , (mkModuleName "GHC.List",   mkVarOcc "tail")
              , (mkModuleName "GHC.List",   mkVarOcc "init")
              , (mkModuleName "GHC.List",   mkVarOcc "last")
              , (mkModuleName "GHC.List",   mkVarOcc "!!")
              , (mkModuleName "Data.Maybe", mkVarOcc "fromJust")
              ]

-- | Checks if the current 'Name' is in the list of the partial functions.
checkName :: ModuleName -> OccName -> Bool
checkName modName occName = or $ map checkFun partialFuns
  where
    checkFun :: (ModuleName, OccName) -> Bool
    checkFun (pModName, pOccName) = (pModName == modName)
                                 && (pOccName == occName)
