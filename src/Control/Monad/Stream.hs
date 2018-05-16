{-# LANGUAGE TypeFamilies #-}
module Control.Monad.Stream where

import Control.Monad.IO.Class

class MonadIO m =>
      MonadStream m
    where
    type Sender m :: * -> *
    type Reciever m :: * -> *
    -- | Create a stream with a end that can only be sent to and one
    -- that can only be read from.
    createStream :: m (Sender m a, Reciever m a)
    send :: a -> Sender m a -> m ()
    recieve :: Reciever m a -> m a
    spawn :: m () -> m ()
