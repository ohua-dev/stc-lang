{-# LANGUAGE TupleSections, TypeFamilies, RankNTypes, InstanceSigs, FlexibleInstances #-}
module Control.Monad.Stream.PinnedChan where

import Control.Concurrent.Chan
import Control.Concurrent
import Control.Arrow ((&&&))
import Control.Monad.IO.Class

import Control.Monad.Stream

import Control.Monad.State.Lazy

type PChanM = StateT Int IO

newtype S a = S { unS :: a -> PChanM () }

instance MonadStream PChanM where
    type Sender PChanM = S
    -- FIXME Receiver
    type Reciever PChanM = PChanM

    createStream :: PChanM (Sender PChanM a, Reciever PChanM a)
    createStream = (S . (liftIO .) . writeChan &&& liftIO . readChan) <$> liftIO newChan

    send :: a -> Sender PChanM a -> PChanM ()
    send a f = (unS f) a

    -- FIXME receive
    recieve :: Reciever PChanM a -> PChanM a
    recieve ac = ac

    spawn :: PChanM () -> PChanM ()
    -- spawn = ChanM . void . forkIO . runChanM
    spawn comp = do
      let ioComp = evalStateT comp 0
      cap    <- get
      liftIO $ putStrLn $ "forking on cap num: " ++ (show cap)
      _      <- liftIO $ forkOn cap ioComp
      put $ cap + 1
      return ()
