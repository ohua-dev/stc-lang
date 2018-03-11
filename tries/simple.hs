-- to see the core output execute like this:
-- ~/.cabal/bin/ghc-core --no-cast --no-asm -- -dsuppress-type-applications -dsuppress-coercions -dsuppress-module-prefixes  -dsuppress-idinfo simple.hs
-- you can actually see that core understands the fine-grained dependencies
-- because it creates accessor functions to the global state and inlines these.

module Simple where

import Control.Monad.State


data S s = S { s1 :: s, s2 :: s, s3 :: s}

type SF s a = a -> StateT (S s) IO a

foo :: SF Int Int
foo x = do
  S s1 s2 s3 <- get
  put $ S (s1+2) s2 s3
  return $ x+2

bar :: SF Int Int
bar x = do
  liftIO $ print ""
  S s1 s2 s3 <- get
  put $ S s1 (s2+3) s3
  return $ x*3

infixr 9 .@.
(.@.) :: SF s a -> SF s a -> SF s a
sf .@. sg = \x ->
              (sg x) >>= sf

someFunc :: Int -> S Int -> IO (Int, S Int)
someFunc x = runStateT ((bar .@. foo) x)
