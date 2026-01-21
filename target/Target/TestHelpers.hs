{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Test helper utilities for building ScriptContexts.
-- This module does NOT have the onchain-contract annotation,
-- so plu-stan rules won't apply to it.
module Target.TestHelpers (
  lendingValidatorTestContext,
) where

import PlutusLedgerApi.V1 (Credential (..), PubKeyHash (..), ScriptHash (..))
import PlutusLedgerApi.V1.Address (pubKeyHashAddress)
import PlutusLedgerApi.V3 (
  Address (..),
  ScriptContext,
  TxOutRef (..),
  )
import PlutusTx qualified as Tx
import Target.ScriptContextBuilder (
  buildScriptContext,
  mkAdaValue,
  withAddress,
  withInlineDatum,
  withOutRef,
  withOutput,
  withSpendingScript,
  withTxOutAddress,
  withTxOutInlineDatum,
  withTxOutValue,
  withValue,
  )

-- Import LoanDatum from PlutusTx module
import Target.PlutusTx (LoanDatum (..))

-- | Example ScriptContext for the lending validator test
lendingValidatorTestContext :: ScriptContext
lendingValidatorTestContext = buildScriptContext $
  withSpendingScript (Tx.toBuiltinData ()) (
    withOutRef (TxOutRef "aabbccdd" 0)
    <> withAddress (scriptAddress $ ScriptHash "deadbeef")
    <> withValue (mkAdaValue 1000000)
    <> withInlineDatum (Tx.toBuiltinData $ LoanDatum
        { repaymentPkh = PubKeyHash "cafebabe"
        , loanAmount = 500000
        })
  )
  <> withOutput (
    withTxOutAddress (pubKeyHashAddress $ PubKeyHash "cafebabe")
    <> withTxOutValue (mkAdaValue 500000)
    <> withTxOutInlineDatum (Tx.toBuiltinData $ LoanDatum
        { repaymentPkh = PubKeyHash "cafebabe"
        , loanAmount = 500000
        })
  )
  where
    scriptAddress :: ScriptHash -> Address
    scriptAddress sh = Address (ScriptCredential sh) Nothing
