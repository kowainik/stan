{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Target.Partial where


stanHead :: [a] -> a
stanHead = head

stanTail :: [a] -> [a]
stanTail = tail

stanInit :: [a] -> [a]
stanInit = init

stanLast :: [a] -> a
stanLast = last

stanAt :: [a] -> a
stanAt xs = xs !! 42

stanCycle :: [a] -> [a]
stanCycle = cycle
