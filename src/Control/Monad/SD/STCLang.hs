{-# LANGUAGE ExplicitForAll #-}

module Control.Monad.SD.STCLang
  ( STCLang
  , liftWithState
  , runSTCLang
  , CollSt(..)
  ) where

import Control.Monad.SD.Case
import Control.Monad.SD.Ohua
import Control.Monad.SD.Smap
import Data.Dynamic2
import Data.StateElement

import Control.DeepSeq (NFData)
import Control.Monad.State as S

data CollSt = CollSt
  { states :: [S]
  , signals :: [IO S]
  }

instance Monoid CollSt where
  mempty = CollSt [] []
  CollSt st1 si1 `mappend` CollSt st2 si2 =
    CollSt (st1 `mappend` st2) (si1 `mappend` si2)

type STCLang a b = StateT CollSt IO (a -> OhuaM b)

liftWithState ::
     (Typeable s, NFData a, NFData s, Show a)
  => IO s
  -> (a -> StateT s IO b)
  -> STCLang a b
liftWithState state stateThread = do
  s0 <- lift state
  l <- S.state $ \s -> (length $ states s, s {states = states s ++ [toS s0]})
  pure $ liftWithIndex l stateThread

runSTCLang :: (NFData b) => STCLang a b -> a -> IO (b, [S])
runSTCLang langComp a = do
  (comp, gs) <- S.runStateT langComp mempty
  runOhuaM (comp a) $ states gs

smapSTC ::
     forall a b. (NFData b, Show a)
  => STCLang a b
  -> STCLang [a] [b]
smapSTC comp = smap <$> comp
