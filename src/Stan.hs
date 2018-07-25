-- | This module introduces GHC plugin for static analysis @Stan@.

module Stan
       ( plugin
       ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)

import FastString (unpackFS)
import HsDecls (HsGroup)
import HsExtension (GhcRn)
import Plugins (CommandLineOption, Plugin (..), defaultPlugin)
import SrcLoc (srcSpanFile)
import TcRnTypes (TcGblEnv (..), TcM)

import Stan.Analysis (stan)
import Stan.Warning (showWarning)

plugin :: Plugin
plugin = defaultPlugin { renamedResultAction = stanPlugin }

stanPlugin :: [CommandLineOption] -> TcGblEnv -> HsGroup GhcRn -> TcM (TcGblEnv, HsGroup GhcRn)
stanPlugin _ tcEnv hsGroup = do
    -- let modulePath = ms_hspp_file modSummary
    let warnings = stan hsGroup
    unless (null warnings) $ do
        -- print the module name
        let fileName = unpackFS $ srcSpanFile $ tcg_top_loc tcEnv
        liftIO $ putStrLn $ "!!! Found " ++ show (length warnings) ++ " warning(s) in " ++ fileName
        fileContent <- liftIO $ readFile fileName
        for_ warnings (liftIO . putStrLn . showWarning fileContent)

    pure (tcEnv, hsGroup)
