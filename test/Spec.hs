{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Main(main) where

import           Control.Lens
import           Data.Default           (Default (def))
import qualified Data.Map               as Map
import           Ledger
import           Ledger.Ada             as Ada
import qualified Plutus.Contract.Test   as PT
import qualified Plutus.Trace.Emulator  as Trace
import qualified Test.Tasty             as T

main :: IO ()
main = putStrLn "Nice"
