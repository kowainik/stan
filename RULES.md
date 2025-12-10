# Plinth Smart Contract Development - Static Analysis Checks

Reference for common Plu-Stan checks and why they matter. Each item highlights the risk and safer patterns.

## Contents

- [Data Handling & Deserialization](#data-handling--deserialization)
- [Value Handling](#value-handling)
- [Equality](#equality)
- [Optional Types](#optional-types)
- [Higher-Order Functions](#higher-order-functions)
- [Bindings](#bindings)
- [Guards](#guards)
- [Integers](#integers)
- [Tooling](#tooling)
- [Validity Interval / POSIX Time Misuse](#validity-interval--posix-time-misuse)

## Data Handling & Deserialization

- **`unsafeFromBuiltinData`:** Potential unbounded datum spam attack. Warn users and either:
  1. Construct the expected output datum and assert the actual output datum matches it (enforces structural integrity by construction).
  2. Explicitly check structural integrity via a custom `checkIntegrity` that verifies field counts and per-field integrity in the underlying `BuiltinData`.
- **PubKeyHash/ScriptHash in fulfillment criteria without constraints:** Potential unsatisfiable constraints.
  - *Example:* lender-chosen `repaymentAddress` in a lending validator.
  ```haskell
  lendingValidator :: ScriptContext -> ()
  lendingValidator ctx =
    case redeemer of
      RepayLoan ->
        let LoanDatum { repaymentAddress, loanAmount, loanEndDate, interestAPT } =
              getOwnInputDatum ownInput
        in if txOutAddress repaymentOutput == repaymentAddress && otherConditionsMet
             then ()
             else error "repayment output failed to satisfy loan terms"
      _ -> error "unsupported redeemer"
  ```
  - **Issue:** Without validating `PubKeyHash`/`ScriptHash`, the lender can set `repaymentAddress` to malformed `BuiltinData` such as `Constr 0 [Constr 0 ["deadbeef"], Constr 1 []]`. It preserves structural integrity but violates ledger `Address` invariants, so the ledger rejects any output to it and the borrower can never repay.

## Value Handling

- **`valueOf` instead of `valueEq` without limits:** Potential Dusk token attack when the number of unique tokens is unbounded.
- **`valueOf` with `adaSymbol` and `adaToken`:** Prefer extracting ADA directly.
  ```haskell
  -- BuiltinData variant
  outAda =
    BI.unsafeDataAsI
      $ BI.snd
      $ BI.head
      $ BI.unsafeDataAsMap
      $ BI.snd
      $ BI.head
      $ BI.unsafeDataAsMap outVal

  -- Or SOP-style
  outAda = snd $ head $ M.toList $ snd $ head (M.toList $ getValue outVal)
  ```
- **`currencySymbolValueOf` on minted value:** Potential unauthorized minting attack (common in `Burn` redeemers).
  ```haskell
  ourMintingPolicy :: ScriptContext -> BuiltinUnit
  ourMintingPolicy ctx =
    case scriptContextRedeemer ctx of
      Mint -> ... -- deposit liquidity for LP tokens
      Burn -> symbolValueOf mintedValue ownCS < 0
    where
      MintingScript ownCS = scriptContextScriptInfo ctx
      mintedValue = txInfoMint (scriptContextTxInfo ctx)
  ```
  - **Issue:** `currencySymbolValueOf` does not enforce that all entries in the value are strictly positive or negative. An attacker can burn and mint in the same transaction:
  ```haskell
  txInfoMint =
    PMap
      [ (PCurrencySymbol ourCS,
          PMap [ (PTokenName "TokenA", -2)
               , (PTokenName "TokenB", 1)
               ]
        )
      ]
  ```
  This passes the `Burn` check while still minting `TokenB`.

## Equality

- **Eq on `ScriptHash` / `PubKeyHash` / `PaymentCredential`:** Potential staking value theft. Prefer equality on full `Address`.
- **Dangerous strict equalities:** Using strict integer equalities that attackers can manipulate.
  - *Example:*
  ```haskell
  -- minUTxOValue from testnet-shelley-genesis.json
  minTxOut :: Integer
  minTxOut = 2_000_000

  fee :: Integer
  fee = 300_000

  checkProposalValue proposalOutput fee =
    let proposalOutValue = txOutValue proposalOutput
        adaAmount = Value.lovelaceValueOf proposalOutValue
    in adaAmount #== minTxOut + fee
  ```
  - **Issue:** `minUTxOValue` is dynamic and set by ledger protocol parameters. Hard-coding it can strand funds if the required minimum rises above the enforced equality.

## Optional Types

- **Use of `Maybe`/`Either` in on-chain code:** Anti-pattern. Prefer fast-fail variants (for example, `tryFind` instead of `find`) or handle the other case via a continuation function.

## Higher-Order Functions

- **Higher-order helpers (`all`, `any`, etc.):** Anti-pattern in on-chain code; prefer specialized versions.
  - *Example:*
  ```haskell
  findOwnInput :: ScriptContext -> Maybe TxInInfo
  findOwnInput ScriptContext{ scriptContextTxInfo = TxInfo{ txInfoInputs }
                            , scriptContextPurpose = Spending txOutRef
                            } =
    find (\TxInInfo{txInInfoOutRef} -> txInInfoOutRef == txOutRef) txInfoInputs
  findOwnInput _ = Nothing
  ```
  Rewritten with a specialized recursive function:
  ```haskell
  findOwnInput :: TxOutRef -> [TxInInfo] -> TxInInfo
  findOwnInput ownOutRef = go
    where
      go [] = error "findOwnInput: not found"
      go (i@TxInInfo{txInInfoOutRef} : rest) =
        if txInInfoOutRef == ownOutRef
          then i
          else go rest
  ```

## Bindings

- **Non-strict `let` bindings used multiple times:** Make them strict to avoid repeated evaluation.
- **Hardcoded values:** Most ledger-dependent values change across hardforks. Avoid hardcoding unless truly constant; prefer dynamic retrieval or explicit acknowledgment that the value is invariant.

## Guards

- **Guard syntax in on-chain code:** Anti-pattern that produces inefficient UPLC. Prefer `if-then-else` or lower-level conditionals.
  ```haskell
  decimalLog2 :: Integer -> Integer
  decimalLog2 n
    | n == 0 = traceError "log2: -inf"
    | n == 1 = 0
    | n `modulo` 2 == 0 = 1 + decimalLog2 (n `divide` 2)
    | otherwise = traceError "log2: non-decimal output"
  ```

## Integers

- **Unconstrained integers:** Attackers can supply negative or out-of-range values. Add explicit range checks (for example, `x > 0`) and prefer newtypes that enforce invariants (such as `Natural`).

## Tooling

- **Codex integration:** Use Codex for vulnerability detection; similar to Slither (Ethereum static analysis).

## Validity Interval / POSIX Time Misuse

- **Detector:** Flags contracts that compare timestamps without checking both bounds of `txInfoValidRange`, equality on slots, or open-ended intervals.
- **Risk:** Unbounded ranges can undermine intended timeboxing logic.
