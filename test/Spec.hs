{-# LANGUAGE OverloadedLists #-}
import Test.Framework

import CorrectnessFuturesBasedMonad as FBM
import CorrectnessStreamsBasedMonad as SBM
import Data.Typeable
import Monad.Generator
import Monad.StreamsBasedExplicitAPI
import Monad.StreamsBasedFreeMonad
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Control.Monad.IO.Class



main :: IO ()
main =
    flip
        defaultMainWithOpts
        mempty
        [FBM.testSuite, SBM.testSuite, basicRuntimeTests]
-- main = flip defaultMainWithOpts mempty FBM.testSuite

simpleLift f = call (liftSf $ sfm . pure . f) united
simpleLift2 f = call (liftSf $ \a b -> sfm $ pure $ f a b) united

basicRuntimeTests :: Test
basicRuntimeTests =
    testGroup
        "Streams based monad runtime and front-end tests"
        [ testCase "using if" $ do
              let a = do
                      four <- sfConst (4 :: Int)
                      five <- sfConst 5
                      isGreater <- (four `gt` five)
                      if_ isGreater (sfConst True) (sfConst False)
              result <- runOhuaM a ()
              assertEqual "If selected incorrect branch" result False
        , testGroup
              "if in smap"
              [ testCase "simple" $ do
                    let p :: Int -> Bool
                        p x = (x `mod` 2) == 0
                        f = (* 2)
                        l = [0 .. 20] :: [Int]
                    let a = do
                            l' <- sfConst l
                            test <- sfConst p
                            flip smap l' $ \val -> do
                                cond <- simpleLift2 ($) test val
                                if_ cond (simpleLift f val) (pure val)
                    result <- runOhuaM a ()
                    assertEqual
                        "Result was not correct"
                        result
                        (map (\x ->
                                  if p x
                                      then f x
                                      else x)
                             l)
              , testCase "with env only branch" $ do
                    result <-
                        flip runOhuaM () $
        --v <- sfConst 8
                         do
                            l <- sfConst [0 .. 30]
                            flip smap l $ \i -> do
                                cond <- simpleLift (\i -> i `mod` 3 == 0) i
                                if_ cond (pure i) (sfConst 8)
                    assertEqual
                        ""
                        result
                        [ if x `mod` 3 == 0
                            then x
                            else 8
                        | x <- [0 .. 30 :: Int]
                        ]
              ]
        , testGroup
              "smapGen"
              [ testCase "mapping a single function" $ do
                    let f = (+ 3)
                    result <-
                        flip runOhuaM () $ do
                            l <- sfConst [0 :: Int .. 10]
                            call (liftSf $ sfm . liftIO . runGenerator) united =<<
                                smapGen (simpleLift f) l
                    assertEqual "" (result :: [Int]) (map f [0 .. 10])
              , testCase "mapping a single function with free variables" $ do
                    let f = (+ 3)
                    result <-
                        flip runOhuaM () $ do
                            l <- sfConst [0 .. 10]
                            f' <- sfConst f
                            call (liftSf $ sfm . liftIO . runGenerator) united =<<
                                smapGen (simpleLift2 ($) f') l
                    assertEqual "" (result :: [Int]) (map f [0 .. 10])
              ]
        ]
