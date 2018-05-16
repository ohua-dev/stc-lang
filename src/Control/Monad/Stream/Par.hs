{-# LANGUAGE TypeFamilies #-}
module Control.Monad.Stream.Par
    ( S
    , ParIO
    , runParIO
    ) where

import Control.Monad.Par.IO as P
import Control.Monad.Par.Class as P
import Data.IORef
import Control.Monad.IO.Class

import Control.Monad.Stream

data Cons a = Cons a (IVar (Cons a)) 

newtype S a = S { unS :: IORef (IVar (Cons a)) }

instance MonadStream ParIO where
    type Sender ParIO = S
    type Reciever ParIO = S
    createStream = do
        v <- new
        in_ <- liftIO $ newIORef v
        out <- liftIO $ newIORef v
        pure (S in_, S out)

    -- This implementation is inherently unsafe. We use IORef and
    -- non-atomic operations to update them. This only works if the
    -- sender and receiver are never accessed simultaneously from two
    -- threads, which shouldn't happen because of the way the runtime
    -- is written. It could be made safe by using a Maybe and atomic
    -- operations, however I expect this will cause slowdown which, as
    -- the runtime should make sure this thing is impossible anyways,
    -- I am not willing to risk.
    send v =
        withS $ \var -> do
            next <- new
            put_ var $ Cons v next
            pure ((), next)
    recieve =
        withS $ \var -> do
            Cons v next <- get var
            pure (v, next)
    spawn = P.fork


withS :: MonadIO m => (IVar (Cons a) -> m (b, IVar (Cons a))) -> S a -> m b
withS ac (S ref) = do
    var <- liftIO $ readIORef ref
    (val, var') <- ac var
    liftIO $ writeIORef ref var'
    pure val
