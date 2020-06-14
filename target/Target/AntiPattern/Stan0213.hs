{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

{- | Sum types pattern matching on @_@ -}

module Target.AntiPattern.Stan0213 where



stanLambda :: Ordering -> String
stanLambda = \case
    EQ -> "EQ"
    _ -> "Other"

stanLambdaLit :: Int -> String
stanLambdaLit = \case
    1 -> "One"
    _ -> "Other"

stanCase :: Ordering -> String
stanCase o = case o of
    EQ -> "EQ"
    LT -> "LT"
    _  -> "Other"

stanCaseLit :: String -> String
stanCaseLit str = case 'x' : str of
    "xAbc" -> "Here"
    "xBcc" -> "There"
    _      -> "Far far away"

stanCaseNo :: Ordering -> String
stanCaseNo o = case o of
    EQ -> "EQ"
    LT -> "LT"
    GT -> "GT"

stanLambdaNo :: Ordering -> String
stanLambdaNo = \case
    EQ -> "EQ"
    LT -> "LT"
    GT -> "GT"

stanMaybeCase :: Int -> Maybe Int -> Int
stanMaybeCase i mb = case mb of
    Just x -> x + 1
    _      -> i + 1

stanMaybeLambdaCase :: Int -> Maybe Int -> Int
stanMaybeLambdaCase i = \case
    Just x -> x + 1
    _      -> i + 1

stanMaybeLambdaCaseNo :: Int -> Maybe Int -> Int
stanMaybeLambdaCaseNo i = \case
    Just x -> x + 1
    Nothing -> i + 1

stanMaybeLambdaCaseFirst_ :: Int -> Maybe Int -> Int
stanMaybeLambdaCaseFirst_ i = \case
    _ -> i + 1

stanLambdaLitChar :: Char -> String
stanLambdaLitChar = \case
    'c' -> "One"
    _ -> "Other"
