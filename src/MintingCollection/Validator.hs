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

module MintingCollection.Validator where (
    mintingValidator
)

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

data MintingValidator
instance Scripts.ValidatorTypes MintingValidator where
    type instance DatumType MintingValidator = ()
    type instance RedeemerType MintingValidator = ()

{-# INLINEABLE mkMintingValidator #-}
mkMintingValidator :: () -> () -> ScriptContext -> Bool
mkMintingValidator _ _ _ = True

typedValidator :: Scripts.TypedValidator MintingValidator
typedValidator = Scripts.mkTypedValidator @MintingValidator
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @()

validator :: Validator
validator = Scripts.validatorScript typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator
