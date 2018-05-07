{-# LANGUAGE RankNTypes, NamedFieldPuns, BangPatterns,
             ExistentialQuantification, FlexibleContexts #-}
module Control.Monad.Par.Scheds.TraceDebuggable where

import Control.Monad.Par
import Control.Monad.Par.Scheds.TraceInternal
import Control.Monad as M hiding (mapM, sequence, join)
import Prelude hiding (mapM, sequence, head,tail)
import Data.IORef
import System.IO.Unsafe
import Control.Concurrent hiding (yield)
import GHC.Conc (numCapabilities)
import Control.DeepSeq
import Control.Exception
import System.IO
import Control.Monad.IO.Class
import GHC.Stack
import Ohua.Util



doesThisBlockIndefinitely :: HasCallStack => IO a -> IO a
doesThisBlockIndefinitely ac =
  ac `catch` \BlockedIndefinitelyOnMVar ->
    error "here"


{-# INLINE runPar_internal #-}
runPar_internal :: Bool -> Par a -> IO a
runPar_internal _doSync x = do
    workpools <- replicateM numCapabilities $ newIORef []
    idle <- newIORef []
    let states =
            [ Sched {no = x, workpool = wp, idle, scheds = states}
            | (x, wp) <- zip [0 ..] workpools
            ]
    (main_cpu, _) <- threadCapability =<< myThreadId
    m <- newEmptyMVar
    forM_ (zip [0 ..] states) $ \(cpu, state) ->
        forkOn cpu $
        (if (cpu /= main_cpu)
             then reschedule state
             else do
                 rref <- newIORef Empty
                 sched _doSync state $
                     runCont (x >>= put_ (IVar rref)) (const Done)
                 readIORef rref >>= doesThisBlockIndefinitely . putMVar m) `catch` \e -> do
            hPutStrLn stderr $ displayException (e :: SomeException)
            throwIO e
    r <- doesThisBlockIndefinitely $ takeMVar m
    case r of
        Full a -> return a
        _ -> error "no result"


-- | Run a parallel, deterministic computation and return its result.
-- 
--   Note: you must NOT return an IVar in the output of the parallel
--   computation.  This is unfortunately not enforced, as it is with
--   `runST` or with newer libraries that export a Par monad, such as
--   `lvish`.
runPar :: Par a -> a
runPar = unsafePerformIO . runPar_internal True

-- | A version that avoids an internal `unsafePerformIO` for calling
--   contexts that are already in the `IO` monad.
--
--   Returning any value containing IVar is still disallowed, as it
--   can compromise type safety.
runParIO :: Par a -> IO a
runParIO = runPar_internal True


reschedule :: Sched -> IO ()
reschedule queue@Sched{ workpool } = do
  e <- atomicModifyIORef workpool $ \ts ->
         case ts of
           []      -> ([], Nothing)
           (t:ts') -> (ts', Just t)
  case e of
    Nothing -> steal queue
    Just t  -> sched True queue t


steal :: Sched -> IO ()
steal q@Sched{ idle, scheds, no=my_no } = do
  -- printf "cpu %d stealing\n" my_no
  go scheds
  where
    go [] = do m <- newEmptyMVar
               r <- atomicModifyIORef idle $ \is -> (m:is, is)
               if length r == numCapabilities - 1
                  then do
                     -- printf "cpu %d initiating shutdown\n" my_no
                     mapM_ (\m -> putMVar m True) r
                  else do
                    done <- takeMVar m
                    if done
                       then do
                         -- printf "cpu %d shutting down\n" my_no
                         return ()
                       else do
                         -- printf "cpu %d woken up\n" my_no
                         go scheds
    go (x:xs)
      | no x == my_no = go xs
      | otherwise     = do
         r <- atomicModifyIORef (workpool x) $ \ ts ->
                 case ts of
                    []     -> ([], Nothing)
                    (x:xs) -> (xs, Just x)
         case r of
           Just t  -> do
              -- printf "cpu %d got work from cpu %d\n" my_no (no x)
              sched True q t
           Nothing -> go xs

instance MonadIO Par where
  liftIO = Par . LiftIO
