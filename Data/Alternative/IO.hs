{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Alternative.IO where

import Control.Applicative
import Control.Exception
import Data.Typeable
import Prelude hiding (catch)

instance Alternative IO where
  empty = goNext
  x <|> y = x `catch` (\(_ :: SomeException) -> y)

data AltIOgoNext = AltIOgoNext deriving (Show, Typeable)

instance Exception AltIOgoNext

goNext :: IO a
goNext = throwIO AltIOgoNext

runAnyOne :: [IO a] -> IO a
runAnyOne = foldr (<|>) goNext
