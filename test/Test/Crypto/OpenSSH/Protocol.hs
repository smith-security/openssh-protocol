{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Crypto.OpenSSH.Protocol where

import qualified Crypto.OpenSSH.Protocol as OpenSSH

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Except (runExceptT)

import           Hedgehog


prop_todo :: Property
prop_todo =
  withTests 1 . property $ do
    success

tests :: IO Bool
tests =
  checkParallel $$(discover)
