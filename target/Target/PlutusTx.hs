{-# LANGUAGE BangPatterns #-}

module Target.PlutusTx (assocMap, unsafeFromBuiltinData, usageOfPTxMaybe, pubKeyHashEq, scriptHashEq, credentialHashEq, credentialHashLe, hoListFilter, hoFoldableLength, nestedMapFilter, guardedLog2, nonStrictLetTwice, strictLetTwice, nonStrictLetUsedInBindings, nonStrictLetUsedInBindingAndBody, valueOfEqInput, valueOfEqLet, valueOfEqReversed, valueOfEqPrefix, valueOfEqSection) where

import qualified PlutusTx as Tx
import qualified PlutusTx.AssocMap as AssocMap
import qualified PlutusTx.Foldable as TxFoldable
import qualified PlutusTx.Maybe as Maybe
import qualified PlutusTx.List as TxList

-- Place for future imports
import PlutusLedgerApi.V1 (Credential (..), PubKeyHash (..), ScriptHash (..))
import qualified PlutusLedgerApi.V1.Value as Value
{-# ANN module ("onchain-contract" :: String) #-}
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--

assocMap :: AssocMap.Map k v
assocMap = AssocMap.unsafeFromList mempty

unsafeFromBuiltinData :: Integer
unsafeFromBuiltinData =
  Tx.unsafeFromBuiltinData (error "tbd")

usageOfPTxMaybe :: Integer
usageOfPTxMaybe = let
  x = Maybe.fromMaybe 0 (Maybe.Just 1)
  in x

pubKeyHashEq :: Bool
pubKeyHashEq = pubKeyHash == pubKeyHash
  where
    pubKeyHash :: PubKeyHash
    pubKeyHash = error "tbd"

scriptHashEq :: Bool
scriptHashEq = scriptHash == scriptHash
  where
    scriptHash :: ScriptHash
    scriptHash = error "tbd"

credentialHashEq :: Bool
credentialHashEq = credentialHash == credentialHash
  where
    credentialHash :: Credential
    credentialHash = error "tbd"

credentialHashLe :: Bool
credentialHashLe = credentialHash < credentialHash
  where
    credentialHash :: Credential
    credentialHash = error "tbd"

hoListFilter :: [Integer]
hoListFilter =
  TxList.filter ((>) 0) [1, 2, 3]

hoFoldableLength :: Integer
hoFoldableLength =
  TxFoldable.length ([1, 2, 3] :: [Integer])

nestedMapFilter :: [Integer]
nestedMapFilter =
  let exList = [1, 2, 3, 4]
  in TxList.map ((+) 100) $ TxList.filter ((>) 0) exList

guardedLog2 :: Integer -> Integer
guardedLog2 n
  | n == 0 = 0
  | n == 1 = 1
  | n `mod` 2 == 0 = 1 + guardedLog2 (n `div` 2)
  | otherwise = n

nonStrictLetTwice :: Integer
nonStrictLetTwice =
  let tup = (1, 2)
  in fst tup + snd tup

strictLetTwice :: Integer
strictLetTwice =
  let !tup = (1, 2)
  in fst tup + snd tup

nonStrictLetUsedInBindings :: Integer
nonStrictLetUsedInBindings =
  let tup = (1, 2)
      a = fst tup
      b = snd tup
  in a + b

nonStrictLetUsedInBindingAndBody :: Integer
nonStrictLetUsedInBindingAndBody =
  let tup = (1, 2)
      a = fst tup
  in a + snd tup

valueOfEqInput :: Bool
valueOfEqInput =
  Value.valueOf inputValue adaCS adaToken == 5000
  where
    inputValue :: Value.Value
    inputValue = Value.singleton adaCS adaToken 5000
    adaCS :: Value.CurrencySymbol
    adaCS = Value.adaSymbol
    adaToken :: Value.TokenName
    adaToken = Value.adaToken

valueOfEqLet :: Bool
valueOfEqLet =
  let adaAmountInput = Value.valueOf inputValue adaCS adaToken
      adaAmountOutput = Value.valueOf outputValue adaCS adaToken
  in adaAmountInput == adaAmountOutput
  where
    inputValue :: Value.Value
    inputValue = Value.singleton adaCS adaToken 5000
    outputValue :: Value.Value
    outputValue = Value.singleton adaCS adaToken 5000
    adaCS :: Value.CurrencySymbol
    adaCS = Value.adaSymbol
    adaToken :: Value.TokenName
    adaToken = Value.adaToken

valueOfEqReversed :: Bool
valueOfEqReversed =
  5000 == Value.valueOf inputValue adaCS adaToken
  where
    inputValue :: Value.Value
    inputValue = Value.singleton adaCS adaToken 5000
    adaCS :: Value.CurrencySymbol
    adaCS = Value.adaSymbol
    adaToken :: Value.TokenName
    adaToken = Value.adaToken

valueOfEqPrefix :: Bool
valueOfEqPrefix =
  (==) (Value.valueOf inputValue adaCS adaToken) 5000
  where
    inputValue :: Value.Value
    inputValue = Value.singleton adaCS adaToken 5000
    adaCS :: Value.CurrencySymbol
    adaCS = Value.adaSymbol
    adaToken :: Value.TokenName
    adaToken = Value.adaToken

valueOfEqSection :: Bool
valueOfEqSection =
  (== 5000) (Value.valueOf inputValue adaCS adaToken)
  where
    inputValue :: Value.Value
    inputValue = Value.singleton adaCS adaToken 5000
    adaCS :: Value.CurrencySymbol
    adaCS = Value.adaSymbol
    adaToken :: Value.TokenName
    adaToken = Value.adaToken
