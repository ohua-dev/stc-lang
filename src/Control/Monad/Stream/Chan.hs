{-# LANGUAGE TupleSections, TypeFamilies, RankNTypes, GeneralizedNewtypeDeriving #-}
module Control.Monad.Stream.Chan where

import Control.Concurrent.Chan
import Control.Concurrent
import Control.Arrow ((&&&))
import Control.Monad (void)
import Control.Monad.IO.Class

import Control.Monad.Stream

newtype ChanM a = ChanM
    { runChanM :: (IO a)
    } deriving (Functor, Applicative, Monad, MonadIO)

newtype S a = S { unS :: a -> ChanM () }

instance MonadStream ChanM where
    type Sender ChanM = S
    type Reciever ChanM = ChanM
    createStream = (S . (liftIO .) . writeChan &&& liftIO . readChan) <$> liftIO newChan
    send a f = (unS f) a
    recieve ac = ac
    spawn = ChanM . void . forkIO . runChanM
