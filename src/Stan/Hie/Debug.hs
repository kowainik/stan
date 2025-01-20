{-# LANGUAGE CPP #-}

module Stan.Hie.Debug (module Compat) where

#if __GLASGOW_HASKELL__ <= 810
import Stan.Hie.Debug810 as Compat
#elif __GLASGOW_HASKELL__ == 900
import Stan.Hie.Debug900 as Compat
#elif __GLASGOW_HASKELL__ == 902
import Stan.Hie.Debug902 as Compat
#elif __GLASGOW_HASKELL__ == 904
import Stan.Hie.Debug902 as Compat
#elif __GLASGOW_HASKELL__ == 906
import Stan.Hie.Debug902 as Compat
#elif __GLASGOW_HASKELL__ == 908
import Stan.Hie.Debug908 as Compat
#elif __GLASGOW_HASKELL__ == 910
import Stan.Hie.Debug908 as Compat
#elif __GLASGOW_HASKELL__ == 912
import Stan.Hie.Debug908 as Compat
#endif
