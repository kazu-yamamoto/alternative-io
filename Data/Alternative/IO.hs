{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
   'IO' as 'Alternative' instance.

   If the left 'IO' monad of ('<|>') causes an error or 'goNext' is used,
   the right 'IO' monad is executed.

   Of course, side effects cannot be rolled back. This means
   that this 'Alternative' instance breaks the 'Alternative' laws.
   But it's common in parsers.

-}

module Data.Alternative.IO where

import Control.Applicative
import Control.Exception
import Data.Typeable
import Prelude hiding (catch)

----------------------------------------------------------------

instance Alternative IO where
  empty = goNext
  x <|> y = x `catch` (\(_ :: SomeException) -> y)

----------------------------------------------------------------

{-| Go to the next 'IO' monad by throwing 'AltIOgoNext'.
-}
goNext :: IO a
goNext = throwIO AltIOgoNext

{-| Run any one 'IO' monad.
-}
runAnyOne :: [IO a] -> IO a
runAnyOne = foldr (<|>) goNext

----------------------------------------------------------------

{-| Exception to control 'Alternative' 'IO'.
-}
data AltIOgoNext = AltIOgoNext deriving (Show, Typeable)

instance Exception AltIOgoNext
