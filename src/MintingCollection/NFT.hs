{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module MintingCollection.NFT where

import           Control.Monad            hiding (fmap)
import qualified Data.Map                 as Map
import           Data.Text                (Text)
import           Data.Void                (Void)
import           Ledger                   hiding (mint, singleton)
import           Ledger.Constraints       as Constraints
import qualified Ledger.Typed.Scripts     as Scripts
import           Ledger.Value             as Value
import           Playground.Contract      (ToSchema, ensureKnownCurrencies,
                                           printJson, printSchemas, stage)
import           Playground.TH            (mkKnownCurrencies,
                                           mkSchemaDefinitions)
import           Playground.Types         (KnownCurrency (..))
import           Plutus.Contract          as Contract
import           Plutus.Trace.Emulator    as Emulator
import qualified Plutus.V1.Ledger.Address as PlutusAddress
import qualified PlutusTx
import           PlutusTx.Prelude         hiding (Semigroup (..), unless)
import           Prelude                  (IO, Semigroup (..), Show (..),
                                           String)
import           Text.Printf              (printf)
import           Wallet.Emulator.Wallet

{-# INLINEABLE mkControlPolicy #-}
mkControlPolicy :: TxOutRef -> () -> ScriptContext -> Bool
mkControlPolicy oref _ ctx = hasUtxo
    where
        txInfo :: TxInfo
        txInfo = scriptContextTxInfo ctx

        hasUtxo :: Bool
        hasUtxo = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs txInfo

controlPolicyInstance :: TxOutRef -> Scripts.MintingPolicy
controlPolicyInstance oref = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkControlPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref

controlPolicyCurSymbol :: TxOutRef -> CurrencySymbol
controlPolicyCurSymbol = scriptCurrencySymbol . controlPolicyInstance


type NFTSchema = Endpoint "initControlPolicy" TokenName

initControlPolicy :: Contract w NFTSchema Text ()
initControlPolicy = do
    pkh <- Contract.ownPaymentPubKeyHash
    utxos <- Contract.utxosAt (PlutusAddress.pubKeyHashAddress (unPaymentPubKeyHash pkh))
    case Map.keys utxos of
        []       -> Contract.logError @String "no utxo found"
        oref : _ -> do
            let controlToken     = Value.singleton (controlPolicyCurSymbol oref) "" 1
                lookups = Constraints.mintingPolicy (controlPolicyInstance oref) <> Constraints.unspentOutputs utxos
                tx      = Constraints.mustMintValue controlToken <> Constraints.mustSpendPubKeyOutput oref
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @String $ printf "forged %s" (show controlToken)

endpoints :: Contract () NFTSchema Text ()
endpoints = initControlPolicy' >> endpoints
        where initControlPolicy' = awaitPromise $ endpoint @"initControlPolicy" $ const initControlPolicy

mkSchemaDefinitions ''NFTSchema

mkKnownCurrencies []

test :: IO ()
test = runEmulatorTraceIO $ do
    let w1 = knownWallet 1
        w2 = knownWallet 2
    h1 <- activateContractWallet w1 endpoints
    h2 <- activateContractWallet w2 endpoints
    callEndpoint @"initControlPolicy" h1 "test"
    callEndpoint @"initControlPolicy" h2 "test3"
    void $ Emulator.waitNSlots 1
