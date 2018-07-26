{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
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
import GHC.Conc (numCapabilities,getNumCapabilities)
import Control.DeepSeq
import Control.Exception
import System.IO
import Control.Monad.IO.Class
import GHC.Stack

import Debug.Trace as T


doesThisBlockIndefinitely :: HasCallStack => IO a -> IO a
doesThisBlockIndefinitely ac =
  ac `catch` \BlockedIndefinitelyOnMVar ->
    error "here"


{-# INLINE runPar_internal #-}
runPar_internal :: Bool -> Par a -> IO a
runPar_internal _doSync x = do
    workpools <- replicateM numCapabilities $ newIORef []
    -- numCaps <- getNumCapabilities
    -- Fix for bug #2: we need to have at least one worker (= 2 capabilites)!
    -- numCaps <- max 2 <$> getNumCapabilities
    -- workpools <- replicateM numCaps $ newIORef []
    putStrLn $ "num states: " ++ (show $ length workpools)
    idle <- newIORef []
    let states =
            [ Sched {no = x, workpool = wp, idle, scheds = states}
            | (x, wp) <- zip [0 ..] workpools
            ]
    m <- newEmptyMVar
    -- bug 1: if we have only 1 capability and the main is currently not located on capability 0
    --        then we deadlock! -> verified!
    --
    (main_cpu, _) <- threadCapability =<< myThreadId
    --
    -- Fix: take the code for the old version even though some data might have to move.
    -- let main_cpu = 0
    putStrLn $ "main cpu: " ++ (show main_cpu)
    forM_ (zip [0 ..] states) $ \(cpu, state) ->
        forkOn cpu $
        (if (cpu /= main_cpu)
             then reschedule state
             else do
                 putStrLn "running main thread: "
                 rref <- newIORef Empty
                 sched _doSync state $
                     -- bug 2: `runCont x` only delivers a trace (which is something like a free monad data structure -> linked list).
                     --        now somebody needs to work that trace.
                     --        but if we have only a single capability which the cpu_main thread is running on then nobody will actually
                     --        work the trace.
                     --        note that `sched` only pushes work to the work queue!
                     --        the code in pushWork adds task to the *front* of the queue. since there are no idle workers none are woken
                     --        up and none will actually steal the work.
                     --        when `sched` now sees the last task (the line added to every Par computation below with Done) then it will
                     --        itself try to run the trace.
                     --        (I did not find out how it eventually reaches Done in `sched` and then just exits out of the scheduler loop.)
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
    Nothing -> T.trace ("stealing something") $ steal queue
    Just t  -> T.trace ("scheduling something") $ sched True queue t


steal :: Sched -> IO ()
steal q@Sched{ idle, scheds, no=my_no } = do
  -- printf "cpu %d stealing\n" my_no
  go scheds
  where
    go [] = do m <- newEmptyMVar
               r <- atomicModifyIORef idle $ \is -> (m:is, is)
               if length r == numCapabilities - 1
               -- --> begin new sebastian
               -- if length r == (length scheds) - 1
               -- numCaps <- getNumCapabilities
               -- if length r == numCaps - 1
               -- <-- end new sebastian
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
