{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Target.Performance where

import Control.Monad.IO.Class (MonadIO)



foo :: (MonadIO m, Functor m) => m ()
foo = undefined

bar :: MonadIO m => Functor m => m ()
bar = undefined
{-# SPECIALIZE bar :: IO () #-}
