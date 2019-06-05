{-# LANGUAGE OverloadedLists #-}
import Test.Framework

import SD.Correctness as FBM
import DD.Correctness as SBM
-- import STMonadStateThreads as STM
import SD.Performance as PFBM
import DD.Performance as PSBFM
import Data.Typeable
import Monad.Generator
import Control.Monad.DD.StreamsBasedExplicitAPI as API
import Control.Monad.DD.StreamsBasedFreeMonad
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Control.Monad.IO.Class
import Lens.Micro (lens, Lens')
import Control.Monad.State (get, put)
import Data.Functor.Identity
import Control.Monad.Trans
import Control.Monad.State

import Control.Monad.Stream.Chan
import Control.Monad.Stream.Par

main :: IO ()
main =
    flip
        defaultMainWithOpts
        mempty
        -- [PFBM.testSuite]
        --[PSBFM.testSuite]
        [FBM.testSuite]
        -- [STM.testSuite]
        -- [FBM.testSuite, SBM.testSuite, basicRuntimeTests, STM.testSuite]
-- main = flip defaultMainWithOpts mempty FBM.testSuite

simpleLift :: (Typeable a, Typeable b) => (a -> b) -> Var a -> ASTM s (Var b)
simpleLift f = call (liftSf $ sfm . pure . f) united
simpleLift2 :: (Typeable a, Typeable b, Typeable c) => (a -> b -> c) -> Var a -> Var b -> ASTM s (Var c)
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
                            l <- sfConst [0 .. 30 :: Int]
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
              "generator"
              [ testGroup
                    "basic functions"
                    [ testCase "iterateState" $ do
                          let expected = [1..10] >>= \i -> [i .. i + 3]

                          let mkReal :: Int -> Generator IO Int
                              mkReal =
                                  evalStateT $ forever $ do
                                      i <- get
                                      if i == 10
                                          then lift finish
                                          else do
                                              modify succ
                                              lift [i .. i + 3]
                          result <- toList $ mkReal (1 :: Int)
                          print result
                          assertEqual "" expected result
                    ]
              , testGroup
                    "mapping"
                    [ testCase "a single function" $ do
                          let f = (+ 3)
                          result <-
                              flip runOhuaM () $ do
                                  l <- sfConst [0 :: Int .. 10]
                                  call
                                      (liftSf $ sfm . liftIO . toList)
                                      united =<<
                                      smapGen (simpleLift f) l
                          assertEqual "" (map f [0 .. 10]) (result :: [Int])
                    , testCase "a single function with free variables" $ do
                          let f = (+ 3)
                          result <-
                              flip runOhuaM () $ do
                                  l <- sfConst [0 .. 10]
                                  f' <- sfConst f
                                  call
                                      (liftSf $ sfm . liftIO . toList)
                                      united =<<
                                      smapGen (simpleLift2 ($) f') l
                          assertEqual "" (map f [0 .. 10]) (result :: [Int])
                    ]
              -- , testGroup
              --       "generating"
              --       [ testCase "generate only" $ do
              --             let l = [1 .. 10]
              --                 idLens :: Lens' [Int] [Int]
              --                 idLens = lens id (\_ a ->  a)
              --             result <-
              --                 flip runOhuaM l $ do
              --                     g <-
              --                         generate $
              --                         call
              --                             (liftSf $
              --                              sfm $ do
              --                                  s <- get
              --                                  case s of
              --                                      x:xs -> do
              --                                          put xs
              --                                          pure $ Just x
              --                                      [] -> pure Nothing)
              --                             idLens
              --                     call (liftSf $ sfm . liftIO . runGenerator) united g
              --             assertEqual "lists differ" result l
              --       ]
              ]
        ]
  where
    runOhuaM a s = runChanM $ API.runOhuaM a s
