{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Alternative.IO.Lifted where

import Control.Applicative
import Control.Exception (Exception, SomeException)
import Control.Exception.Lifted (catch, throwIO)
import Control.Monad.Base (MonadBase)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Typeable
import Prelude hiding (catch)
import Control.Monad.IO.Class

instance (MonadIO m, MonadBaseControl IO m) => Alternative m where
  empty = goNext
  x <|> y = x `catch` (\(_ :: SomeException) -> y)

data AltIOLiftedGoNext = AltIOLiftedGoNext deriving (Show, Typeable)

instance Exception AltIOLiftedGoNext

goNext :: (MonadIO m, MonadBase IO m) => m a
goNext = throwIO AltIOLiftedGoNext

runAnyOne :: (MonadIO m, MonadBaseControl IO m) => [m a] -> m a
runAnyOne = foldr (<|>) goNext
