-- to see the core output execute like this:
-- ~/.cabal/bin/ghc-core --no-cast --no-asm -- -dsuppress-type-applications -dsuppress-coercions -dsuppress-module-prefixes  -dsuppress-idinfo simple.hs
-- you can actually see that core understands the fine-grained dependencies
-- because it creates accessor functions to the global state and inlines these.

module Simple where

import Control.Monad.State
import SfOperations

foo :: SF Int Int
foo x = do
  S s1 s2 s3 <- get
  put $ S (s1+2) s2 s3
  return $ x+2

bar :: SF Int Int
bar x = do
  -- liftIO $ print ""
  S s1 s2 s3 <- get
  put $ S s1 (s2+3) s3
  return $ x*3

-- someFunc :: Int -> S Int -> IO (Int, S Int)
-- someFunc x = runStateT ((bar .@. foo) x)

someFunc :: Int -> [Int] -> (Int, [Int])
someFunc x = runState (((sf 1 bar) .@. (sf 0 foo)) x)

smapTest :: [Int] -> IO ()
smapTest xs = do
  print $ runState (smap ((sf 1 bar) .@. (sf 0 foo)) xs) [0,0]
