{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -O2 #-}

module ExtensibleEffects where

import Control.Eff (Eff, Lift, runLift)
import Control.Eff.Extend (send)
import Control.Monad (replicateM_, void)
import System.IO (Handle, hPrint)
import System.IO.Temp (withSystemTempFile)
import Template (declMany)

$(declMany 10)

benchEE :: IO ()
benchEE = withSystemTempFile "result.txt" $ \_ h ->
  runBench h $
    replicateM_ (10000 :: Int) $ do
      send $ M1 1
      send $ M2 2
      send $ M3 3
      send $ M4 4
      send $ M5 5
      send $ M6 6
      send $ M7 7
      send $ M8 8
      send $ M9 9
      send $ M10 10

benchIO :: IO ()
benchIO = withSystemTempFile "result.txt" $ \_ h ->
  replicateM_ (10000 :: Int) $ do
    void $ pure (1 :: Int)
    void $ pure (2 :: Int)
    void $ pure (3 :: Int)
    void $ pure (4 :: Int)
    void $ pure (5 :: Int)
    void $ pure (6 :: Int)
    void $ pure (7 :: Int)
    void $ pure (8 :: Int)
    void $ pure (9 :: Int)
    void $ pure (10 :: Int)

benchIOBaseline :: IO ()
benchIOBaseline = withSystemTempFile "result.txt" $ \_ _ -> pure ()

runM1M2 :: Handle -> Eff [M2, M1, Lift IO] w -> IO w
runM1M2 h = runLift . runM1 h . runM2 h

runBench :: Handle -> Eff '[M10, M9, M8, M7, M6, M5, M4, M3, M2, M1, Lift IO] w -> IO w
runBench h =
  runLift . runM1 h . runM2 h . runM3 h . runM4 h . runM5 h . runM6 h . runM7 h . runM8 h . runM9 h . runM10 h
