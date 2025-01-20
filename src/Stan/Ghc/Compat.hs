{-# LANGUAGE CPP #-}

module Stan.Ghc.Compat (module Compat) where

#if __GLASGOW_HASKELL__ <= 810
import Stan.Ghc.Compat810 as Compat
#elif __GLASGOW_HASKELL__ == 900
import Stan.Ghc.Compat900 as Compat
#elif __GLASGOW_HASKELL__ == 902
import Stan.Ghc.Compat902 as Compat
#elif __GLASGOW_HASKELL__ == 904
import Stan.Ghc.Compat902 as Compat
#elif __GLASGOW_HASKELL__ == 906
import Stan.Ghc.Compat906 as Compat
#elif __GLASGOW_HASKELL__ == 908
import Stan.Ghc.Compat906 as Compat
#elif __GLASGOW_HASKELL__ == 910
import Stan.Ghc.Compat906 as Compat
#elif __GLASGOW_HASKELL__ == 912
import Stan.Ghc.Compat906 as Compat
#endif
