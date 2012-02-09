{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

{-|
   Lifted 'IO' like 'Alternative' instance. Due to technical limitation,
   Lifted 'IO' is not an instance of 'Alternative'. ('<||>') is provided
   instead.
-}

module Data.Alternative.IO.Lifted where

import Control.Exception (Exception, SomeException)
import Control.Exception.Lifted (catch, throwIO)
import Control.Monad.Base (MonadBase)
import Control.Monad.IO.Class
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Typeable
import Prelude hiding (catch)

{-| If the left 'IO' monad of ('<||>') causes an error or 'goNext' is used,
   the right 'IO' monad is executed.
-}
(<||>) :: MonadBaseControl IO m => m a -> m a -> m a
x <||> y = x `catch` (\(_ :: SomeException) -> y)

{-| Go to the next 'IO' monad by throwing 'AltIOLiftedGoNext'.
-}
goNext :: (MonadIO m, MonadBase IO m) => m a
goNext = throwIO AltIOLiftedGoNext

{-| Run any one lifted 'IO' monad.
-}
runAnyOne :: (MonadIO m, MonadBaseControl IO m) => [m a] -> m a
runAnyOne = foldr (<||>) goNext

data AltIOLiftedGoNext = AltIOLiftedGoNext deriving (Show, Typeable)

instance Exception AltIOLiftedGoNext
