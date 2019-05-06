{-# LANGUAGE BangPatterns, CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Mandelbrot(mandelBench) where
{- Code taken from: https://github.com/simonmar/monad-par/blob/master/examples/src/mandel/mandel.hs-}

import Control.Monad
import Control.DeepSeq
import Control.Exception
import Data.Complex
import System.Environment
import System.IO
#ifdef ALIST
import Control.Monad.Par.AList as A
#endif

#ifdef PARSCHED
import PARSCHED
#else
import Control.Monad.Par
#endif

import qualified Data.Vector.Unboxed as V

#ifdef WRITE_IMAGE
import Codec.Picture  -- JuicyPixels
import qualified Data.Vector.Storable as V
#endif

#ifdef NEW_GENERIC
import qualified Data.Par as C
import qualified Data.Par.Range as C
import Control.Par.Class
parMapReduceRangeThresh :: (NFData a, ParFuture p, FutContents p a)
                        => Int -> C.InclusiveRange -> (Int -> p a) -> (a -> a -> p a) -> a -> p a
parMapReduceRangeThresh = C.parMapReduceThresh
#else
import Control.Monad.Par.Combinator as C
#endif

import BenchLib
import Monad.FuturesBasedMonad as Ohua
import Control.Monad.Identity (runIdentity)

mandel :: Int -> Complex Double -> Int
mandel max_depth c = loop 0 0
  where
   fn = magnitude
   loop i !z
    | i == max_depth = i
    | fn(z) >= 2.0   = i
    | otherwise      = loop (i+1) (z*z + c)


threshold = 1


data VecTree = Leaf (V.Vector Int)
	     | MkNode VecTree VecTree
  deriving (Eq,Show)

#if 0
instance V.Unbox a => NFData (V.Vector a) where
  rnf v = rnf (V.length v)
#endif

instance NFData VecTree where
  rnf (Leaf v) = rnf v
  rnf (MkNode v1 v2) = rnf v1 `seq` rnf v2

leftmost (Leaf v)     = v
leftmost (MkNode l _) = leftmost l

#ifdef ALIST
runMandel :: Double -> Double -> Double -> Double -> Int -> Int -> Int -> Par (AList [Int])
runMandel minX minY maxX maxY winX winY max_depth = do
  -- This version does a ROW at a time in parallel:
  A.parBuildThreshM threshold (C.InclusiveRange 0 (winY-1)) $ \y ->
       do let l = [ mandelStep y x | x <- [0.. winX-1] ]
          deepseq l (return l)

#else
-- RRN: Comment to handle LVish types too:
-- runMandel :: Double -> Double -> Double -> Double -> Int -> Int -> Int -> Par VecTree
runMandel minX minY maxX maxY winX winY max_depth runner = do
  -- Auto-partitioning version.  A bit worse:
  -- C.parMapReduceRange (C.InclusiveRange 0 (winY-1))
  runner threshold (C.InclusiveRange 0 (winY-1))
     (\y ->
       do
          let vec = V.generate winX (\x -> mandelStep y x)
          seq (vec V.! 0) $
           return (Leaf vec))
     (\ a b -> return$ MkNode a b)
     (Leaf V.empty)
#endif
  where
    mandelStep i j = mandel max_depth z
        where z = ((fromIntegral j * r_scale) / fromIntegral winY + minY) :+
                  ((fromIntegral i * c_scale) / fromIntegral winX + minX)
    r_scale  =  maxY - minY  :: Double
    c_scale =   maxX - minX  :: Double

-- pure version of the above
runMandelP minX minY maxX maxY winX winY max_depth runner =
  -- Auto-partitioning version.  A bit worse:
  -- C.parMapReduceRange (C.InclusiveRange 0 (winY-1))
  runner threshold (C.InclusiveRange 0 (winY-1))
     (\y ->
        let vec = V.generate winX (\x -> mandelStep y x)
        in seq (vec V.! 0) $ (Leaf vec))
     (\ a b ->  MkNode a b)
     (Leaf V.empty)
  where
    mandelStep i j = mandel max_depth z
        where z = ((fromIntegral j * r_scale) / fromIntegral winY + minY) :+
                  ((fromIntegral i * c_scale) / fromIntegral winX + minX)
    r_scale  =  maxY - minY  :: Double
    c_scale =   maxX - minX  :: Double

-- runMandelP_ minX minY maxX maxY winX winY max_depth runner =
--   -- Auto-partitioning version.  A bit worse:
--   -- C.parMapReduceRange (C.InclusiveRange 0 (winY-1))
--   runner threshold (C.InclusiveRange 0 (winY-1))
--      (\y ->
--         let vec = V.generate winX (\x -> mandelStep y x)
--         in seq (vec V.! 0) $ (Leaf vec))
--      (\ a b ->  MkNode a b)
--      (Leaf V.empty)
--   where
--     mandelStep i j = mandel max_depth z
--         where z = ((fromIntegral j * r_scale) / fromIntegral winY + minY) :+
--                   ((fromIntegral i * c_scale) / fromIntegral winX + minX)
--     r_scale  =  maxY - minY  :: Double
--     c_scale =   maxX - minX  :: Double

#ifdef WRITE_IMAGE
-- makeImage :: Integer -> Integer -> Int -> A.AList [Int] -> Image PixelRGB8
makeImage :: Int -> Int -> Int -> A.AList [Int] -> Image PixelRGB8
makeImage x y depth ls =
  -- This is a quite silly series of intermediate structures:
  Image x y (V.fromList $ concat $
	     Prelude.map prettyRGB $
	     concat$ A.toList ls)
 where
   prettyRGB s =
      let t = (fromIntegral (depth - s)) in
--      PixelRGB8 (fromIntegral s) t t
      [fromIntegral s, t, t]
#endif

simple x y depth = runMandel (-2) (-2) 2 2 x y depth
simpleP :: Monad m => Int -> Int -> Int -> (
    Int -- ^ threshold
    -> InclusiveRange -- ^ range over which to calculate
    -> (Int -> VecTree) -- ^ compute one result
    -> (VecTree -> VecTree -> VecTree) -- ^ combine two results (associative)
    -> VecTree -- ^ initial result
    -> m VecTree) -> m VecTree
simpleP x y depth = runMandelP (-2) (-2) 2 2 x y depth

-- simpleP_ :: Int -> Int -> Int -> (Int -- ^ threshold
--     -> InclusiveRange -- ^ range over which to calculate
--     -> (Int -> VecTree) -- ^ compute one result
--     -> (VecTree -> VecTree -> VecTree) -- ^ combine two results (associative)
--     -> VecTree -- ^ initial result
--     -> IO VecTree) -> IO VecTree
-- simpleP_ x y depth = runMandelP_ (-2) (-2) 2 2 x y depth

--------------------------------------------------------------------------------

#ifdef ALIST
-- A meaningless checksum.  This match the C++ CnC benchmark:
mandelCheck :: AList [Int] -> Int -> Int -> Int
mandelCheck als max_col max_depth = loop 0 als 0
 where
 -- Sum positions of the pixels that are equal to max_depth:
 loop i als !sum | A.null als = sum
 loop i als !sum = loop (i+1) (A.tail als)
		        (loop2 i 0 (A.head als) sum)
 loop2 i j []    !sum = sum
 loop2 i j (h:t) !sum | h == max_depth = loop2 i (j+1) t (sum + i*max_col + j)
		      | otherwise      = loop2 i (j+1) t  sum
#endif

 -- This kind of checksum is much simpler:
checkSum :: VecTree -> Int
checkSum (Leaf vec) = V.foldl (+) 0 vec
checkSum (MkNode v1 v2) = checkSum v1 + checkSum v2


-- main = do args <- getArgs
--
--           let (x,y,depth) =
-- 		case args of
-- 		 [] ->
--     --                 runPar $ simple 3 3 3
--                        -- Defaults recommended by Simon Marlow
-- 		       -- (1024,1024,256)
--                        -- Note, this should do something very *quick* when called with no args:
--                        (3,3,3)
--
-- 		 [x,y,depth] ->
--     --		   simple (read x) (read y) (read depth)
-- 		       (read x, read y, read depth)
--
-- 		 -- [minX,minY,maxX,maxY,winX,winY,depth] ->
-- 		 --    runPar $
-- 		 -- 	    runMandel (read minX) (read minY)
-- 		 -- 		      (read maxX) (read maxY)
-- 		 -- 		      (read winX) (read winY) (read depth)
--
--           let pixels = runPar$ simple x y depth parMapReduceRangeThresh
--
--           putStrLn$ "About to poke pixels to launch runPar:"
--           evaluate pixels
--           putStrLn$ "Should be done with runpar at this point."
--
-- #ifdef WRITE_IMAGE
-- 	  writePng "mandel_image.png" (makeImage (fromIntegral x) (fromIntegral y) depth pixels)
-- 	  putStrLn$ "File written."
--           putStrLn$ "Checksum " ++ show (checkSumpixels y depth)
-- #endif
--           putStrLn$ "Spot check, length of leftmost leaf: " ++ show (V.length (leftmost pixels))
--           return ()

parMapReduceRangeThreshSeq:: Int -- ^ threshold
  -> InclusiveRange -- ^ range over which to calculate
  -> (Int -> VecTree) -- ^ compute one result
  -> (VecTree -> VecTree -> VecTree) -- ^ combine two results (associative)
  -> VecTree -- ^ initial result
  -> IO VecTree
parMapReduceRangeThreshSeq t (InclusiveRange mi ma) fn binop init =
  return $ loop mi ma
  where
    loop mi ma
      | ma - mi <= t =
        let mapred a b = let x = fn b
                             result = a `binop` x
                          in result
         in foldl mapred init [mi .. ma]
      | otherwise =
        let mid = mi + ((ma - mi) `quot` 2)
            r = loop (mid + 1) ma
            l = loop mi mid
         in l `binop` r

mandelBench =
  bgroup
    "mandel-bench"
    [ bench "sequential" (nfIO (simpleP x y depth parMapReduceRangeThreshSeq))
    , bench "ohua" (nfIO $ simpleP x y depth Ohua.parMapReduceRangeThresh)
    , bench "par" (nf (runPar . (simple x y depth)) C.parMapReduceRangeThresh)
    ]
  where
    {- Values taken from: https://github.com/simonmar/monad-par/blob/master/examples/src/run_benchmark.hs-}
    -- desktop values
    -- x = 1024
    -- y = 1024
    -- depth = 256
    -- server values
    x = 1024
    y = 1024
    depth = 256
{-
[2012.03.08] {Updated to use Vector}
Looks like this was out of date in that it was still using AList.
AList has never performed well enough, and sure enough, this test was
falling to ~50% productivity at >16 threads.
However, simply replacing AList with vector is no good (14.4s for 512
512 2048 as opposed to 15.5).  It is of course a quadratic copy at
that point.
-}
