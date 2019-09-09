module Control.Monad.SD.FRP
    ( liftSignal
    , runSignals
    , filterSignalM
    , filterSignal
    , Signals
    ) where

import Control.Monad.Generator
import Control.Monad.SD.Case
import Control.Monad.SD.Ohua
import Control.Monad.SD.STCLang
import Control.Monad.SD.Smap
import Data.Dynamic2
import Data.StateElement

import qualified Control.Concurrent as Conc
import qualified Control.Concurrent.BoundedChan as BC
import Control.DeepSeq (NFData)
import Control.Exception (bracket)
import Control.Monad.State as S
import System.IO (hPutStrLn, stderr)

instance Show S where
    show _ = "S"

type Signal = IO

type Signals = (Int, S)

liftSignal :: (Typeable a, NFData a) => Signal a -> IO a -> STCLang Signals a
liftSignal s0 init = do
    idx <-
        S.state $ \s@CollSt {signals} ->
            (length signals, s {signals = signals ++ [toS <$> s0]})
    liftWithState init $ \(i, s) ->
        if i == idx
            then do
                let my = fromS s
                S.put my
                pure my
            else S.get

runSignals :: NFData a => STCLang Signals a -> IO ([a], [S])
runSignals comp = do
    (comp', s) <- S.runStateT comp mempty
    chan <- BC.newBoundedChan 100
    bracket
        (do forM (zip [0 ..] $ signals s) $ \(idx, sig) ->
                Conc.forkIO $
                forever $ do
                    event <- sig
                    BC.writeChan chan $ Just (idx, event))
        (\threads -> mapM_ Conc.killThread threads)
        (\_ -> do
             let signalGen = ioReaderToGenerator (BC.readChan chan)
             runOhuaM (smapGen comp' signalGen) $ states s)

filterSignalM ::
       (Show b, NFData a, NFData b)
    => (a -> OhuaM Bool)
    -> (a -> OhuaM b)
    -> STCLang a (Maybe b)
filterSignalM cond f =
    pure $ \item -> if_ (cond item) (Just <$> f item) (pure Nothing)
    -- | @filter init p f@ applies @f@ to only those values @a@ that satisfy the
    -- predicate @p@. For values not satisfying it returns the last computed value
    -- (initially @init@)

filterSignal ::
       (Show b, Typeable b, NFData b, NFData a)
    => IO b -- Initial value for the output
    -> (a -> OhuaM Bool) -- predicate
    -> (a -> OhuaM b) -- computation to perform on `a`
    -> STCLang a b
filterSignal init cond f = do
    g <- liftWithState init $ maybe S.get (\i -> S.put i >> pure i)
    fil <- filterSignalM cond f
    return $ fil >=> g
