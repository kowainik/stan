module Target.PlutusTx where

import qualified PlutusTx as Tx
import qualified PlutusTx.AssocMap as AssocMap
import qualified PlutusTx.Maybe as Maybe

-- Place for future imports
import PlutusLedgerApi.V1 (Credential (..), PubKeyHash (..), ScriptHash (..))
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
  Tx.unsafeFromBuiltinData (error "we don't care")

usageOfPTxMaybe :: Integer
usageOfPTxMaybe = let
  x = Maybe.fromMaybe 0 (Maybe.Just 1)
  in x

pubKeyHashEq :: Bool
pubKeyHashEq = pubKeyHash == pubKeyHash
  where
    pubKeyHash :: PubKeyHash
    pubKeyHash = error "we don't care"

scriptHashEq :: Bool
scriptHashEq = scriptHash == scriptHash
  where
    scriptHash :: ScriptHash
    scriptHash = error "we don't care"

credentialHashEq :: Bool
credentialHashEq = credentialHash == credentialHash
  where
    credentialHash :: Credential
    credentialHash = error "we don't care"

credentialHashLe :: Bool
credentialHashLe = credentialHash < credentialHash
  where
    credentialHash :: Credential
    credentialHash = error "we don't care"
