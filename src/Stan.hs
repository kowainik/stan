{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Main running module.
-}

module Stan
       ( runStan
       ) where

import Stan.Cli (runStanCli)


runStan :: IO ()
runStan = runStanCli >>= \_ -> pass
