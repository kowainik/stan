{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Target.AntiPattern.Stan0214 where


isEq :: Int -> Int -> Bool
isEq x y
    | x < y = False
    | x > y = False
    | otherwise = True

isEq2 :: String -> String -> Bool
isEq2 s1 s2
    | s1 == s2 = True
    | s1 < s2 = False
    | otherwise = True

weirdEq :: Int -> Int -> Bool
weirdEq a b
   | a == b = True
   | a - 1 == b + 1 = True  -- almost equal
   | otherwise = False

geqOrEq :: Int -> Int -> Bool
geqOrEq a b
   | a >= b = True
   | otherwise = False

inRange :: Int -> Bool
inRange i
   | i >= 100  = False
   | i >= 80   = True
   | i >= 60   = False
   | i >= 40   = True
   | otherwise = False

data Tree a = Leaf | Node a (Tree a) (Tree a)

insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = Node x Leaf Leaf
insert x node@(Node y l r)
    | x < y = Node y (insert x l) r
    | x > y = Node y l (insert x r)
    | otherwise = node
