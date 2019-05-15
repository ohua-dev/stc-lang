module Control.Monad.SD.Combinator where

import Control.Monad.SD.Ohua
  ----
  -- The below comes originally from: https://hackage.haskell.org/package/monad-par-extras-0.3.3/docs/src/Control-Monad-Par-Combinator.html#parMapReduceRangeThresh
  ----
  -- | Computes a binary map\/reduce over a finite range.  The range is
  -- recursively split into two, the result for each half is computed in
  -- parallel, and then the two results are combined.  When the range
  -- reaches the threshold size, the remaining elements of the range are
  -- computed sequentially.
  --
  -- For example, the following is a parallel implementation of
  --
  -- >  foldl (+) 0 (map (^2) [1..10^6])
  --
  -- > parMapReduceRangeThresh 100 (InclusiveRange 1 (10^6))
  -- >        (\x -> return (x^2))
  -- >        (\x y -> return (x+y))
  -- >        0
  --
  -- parMapReduceRangeThresh ::
  --      (NFData a, ParFuture iv p)
  --   => Int -- ^ threshold
  --   -> InclusiveRange -- ^ range over which to calculate
  --   -> (Int -> p a) -- ^ compute one result
  --   -> (a -> a -> p a) -- ^ combine two results (associative)
  --   -> a -- ^ initial result
  --   -> p a
  -- parMapReduceRangeThresh threshold range fn binop init =
  -- loop min max
  -- where
  --   loop min max
  --     | max - min <= threshold =
  --       let mapred a b = do
  --             x <- fn b
  --             result <- a `binop` x
  --             return result
  --        in foldM mapred init [min .. max]
  --     | otherwise = do
  --       let mid = min + ((max - min) `quot` 2)
  --       rght <- spawn $ loop (mid + 1) max
  --       l <- loop min mid
  --       r <- get rght
  --       l `binop` r

instance Show InclusiveRange

mapReduceRangeThresh ::
     (NFData a, Typeable a, Show a)
  => Int -- ^ threshold
  -> InclusiveRange -- ^ range over which to calculate
  -> (Int -> a) -- ^ compute one result
  -> (a -> a -> a) -- ^ combine two results (associative)
  -> a -- ^ initial result
  -> IO a
mapReduceRangeThresh threshold range fn binop init
    -- sadly I could not use STCLang to build this :(
    -- reason: it must be STCLang a b to implement liftSignal instead of just
    -- STCLang b just like OhuaM b
    -- (_, [reduceState]) <- runOhuaM mapReduce [toS init]
    -- return $ fromS reduceState
 = do
  (_, [reduceState]) <- runSTCLang mapReduce chunkGenerator
  return $ fromS reduceState
  where
    mapReduce = do
      reduceST <- liftWithState (return init) reduce
        -- return $\x -> smapGen ((pure . mapAndCombine) >=> reduceST) x
      return $ smapGen ((pure . mapAndCombine) >=> reduceST)
      -- mapReduce = do
      --   smapGen
      --     ((pure . mapAndCombine) >=> liftWithIndexS 0 reduce)
      --     chunkGenerator
    chunkGenerator :: Generator IO InclusiveRange
    chunkGenerator =
      flip stateToGenerator range $ do
        (InclusiveRange mi ma) <- S.get
        if mi >= ma
          then return Nothing
          else let mi' = min (mi + threshold) ma
                in do S.put $ InclusiveRange (mi' + 1) ma
                      return $ Just $ InclusiveRange mi mi'
    list (InclusiveRange mi ma)
      | mi >= ma = []
      | otherwise = InclusiveRange mi mi' : list (InclusiveRange (mi' + 1) ma)
      where
        mi' = min (mi + threshold) ma
    mapAndCombine (InclusiveRange mi ma) =
      let mapred a b =
            let x = fn b
                result = a `binop` x
             in result
       in List.foldl mapred init [mi .. ma]
    reduce v = S.get >>= (S.put . (`binop` v))
  --{-# INLINE parMapReduceRangeThresh #-}
  -- streams output from the map phase to the reduce phase

mapReduce ::
     (NFData a, NFData b, Typeable b, Show a, Show b)
  => (a -> b)
  -> (b -> b -> b)
  -> b
  -> [a]
  -> IO b
mapReduce mapper reducer init xs = do
  (_, [reduceState]) <- runOhuaM algo [toS init]
  return $ fromS reduceState
  where
    algo =
      smapGen (pure . mapper >=> liftWithIndexS 0 reduce) $ listGenerator xs
    reduce v = S.get >>= (S.put . (`reducer` v))
  --{-# INLINE mapReduce #-}
