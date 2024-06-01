{-# LANGUAGE CPP #-}

module Stan.Hie.Compat (module Compat) where

#if __GLASGOW_HASKELL__ <= 810
import Stan.Hie.Compat810 as Compat
#elif __GLASGOW_HASKELL__ == 900
import Stan.Hie.Compat900 as Compat
#elif __GLASGOW_HASKELL__ == 902
import Stan.Hie.Compat902 as Compat
#elif __GLASGOW_HASKELL__ == 904
import Stan.Hie.Compat904 as Compat
#elif __GLASGOW_HASKELL__ == 906
import Stan.Hie.Compat904 as Compat
#elif __GLASGOW_HASKELL__ == 908
import Stan.Hie.Compat904 as Compat
#elif __GLASGOW_HASKELL__ == 910
import Stan.Hie.Compat904 as Compat
#endif
