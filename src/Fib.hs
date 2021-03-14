{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -ddump-to-file -ddump-simpl #-}

module Fib where

import qualified Control.Eff as E
import Control.Monad.Skeleton (MonadView (Return), Skeleton, deboneBy)
import Control.Monad.Trans.Accum (runAccum)
import Control.Monad.Trans.Cont (runCont)
import Control.Monad.Trans.Except (runExcept)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import qualified Control.Monad.Trans.RWS.CPS as C
import qualified Control.Monad.Trans.RWS.Lazy as L
import qualified Control.Monad.Trans.RWS.Strict as S
import Control.Monad.Trans.Reader (ReaderT (runReaderT), runReader)
import Control.Monad.Trans.Select (runSelect)
import qualified Control.Monad.Trans.State.Lazy as L
import qualified Control.Monad.Trans.State.Strict as S
import qualified Control.Monad.Trans.Writer.CPS as C
import qualified Control.Monad.Trans.Writer.Lazy as L
import qualified Control.Monad.Trans.Writer.Strict as S
import qualified Data.Extensible.Effect as Skeleton
import Data.Functor.Identity (Identity (runIdentity))
import RIO (runRIO)

xor :: Bool -> Bool -> Bool
xor True x = not x
xor False x = x

fib :: Monad m => Int -> m Bool
fib = go
  where
    go n
      | n <= 1 = pure $! n == 1
      | otherwise = do
        r1 <- fib (n -1)
        r2 <- fib (n -2)
        pure $! r1 `xor` r2

{-
strictFib :: Monad m => Int -> m Bool
strictFib = go
  where
    go n
      | n <= 1 = pure $ n == 1
      | otherwise = do
        !r1 <- fib (n -1)
        !r2 <- fib (n -2)
        pure $! r1 `xor` r2
        -}

{-# NOINLINE slowFib #-}
slowFib :: Monad m => Int -> m Bool
slowFib = fib

fibIdentity :: Int -> Bool
fibIdentity = runIdentity . fib

fibIO :: Int -> IO Bool
fibIO = fib

fibMaybe :: Int -> Maybe Bool
fibMaybe = fib

fibList :: Int -> [Bool]
fibList = fib

fibReader :: Int -> Bool
fibReader = flip fib ()

fibMaybeT :: Int -> Maybe Bool
fibMaybeT = runIdentity . runMaybeT . fib

fibEither :: Int -> Either () Bool
fibEither = fib

fibExceptT :: Int -> Either () Bool
fibExceptT = runExcept . fib

fibStateST :: Int -> (Bool, ())
fibStateST = (`S.runState` ()) . fib

fibStateLT :: Int -> (Bool, ())
fibStateLT = (`L.runState` ()) . fib

fibReaderT :: Int -> Bool
fibReaderT = (`runReader` ()) . fib

fibContT :: Int -> Bool
fibContT = (`runCont` id) . fib

fibWriterLT :: Int -> (Bool, ())
fibWriterLT = L.runWriter . fib

fibWriterCT :: Int -> (Bool, ())
fibWriterCT = C.runWriter . fib

fibWriterST :: Int -> (Bool, ())
fibWriterST = S.runWriter . fib

fibRWSST :: Int -> (Bool, (), ())
fibRWSST x = S.runRWS (fib x) () ()

fibRWSLT :: Int -> (Bool, (), ())
fibRWSLT x = L.runRWS (fib x) () ()

fibRWSCT :: Int -> (Bool, (), ())
fibRWSCT x = C.runRWS (fib x) () ()

fibSelectT :: Int -> Bool
fibSelectT = (`runSelect` const ()) . fib

fibAccumT :: Int -> (Bool, ())
fibAccumT = (`runAccum` ()) . fib

fibEff :: Int -> Bool
fibEff = E.run . fib

fibEffSkeleton :: Int -> Bool
fibEffSkeleton = Skeleton.leaveEff . fib

fibRIO :: Int -> IO Bool
fibRIO = runRIO () . fib

slowFibIdentity :: Int -> Bool
slowFibIdentity = runIdentity . slowFib

slowFibIO :: Int -> IO Bool
slowFibIO = slowFib

slowFibMaybe :: Int -> Maybe Bool
slowFibMaybe = slowFib

slowFibList :: Int -> [Bool]
slowFibList = slowFib

slowFibReader :: Int -> Bool
slowFibReader = flip slowFib ()

slowFibMaybeT :: Int -> Maybe Bool
slowFibMaybeT = runIdentity . runMaybeT . slowFib

slowFibEither :: Int -> Either () Bool
slowFibEither = slowFib

slowFibExceptT :: Int -> Either () Bool
slowFibExceptT = runExcept . slowFib

slowFibStateST :: Int -> (Bool, ())
slowFibStateST = (`S.runState` ()) . slowFib

slowFibStateLT :: Int -> (Bool, ())
slowFibStateLT = (`L.runState` ()) . slowFib

slowFibReaderT :: Int -> Bool
slowFibReaderT = (`runReader` ()) . slowFib

slowFibContT :: Int -> Bool
slowFibContT = (`runCont` id) . slowFib

slowFibWriterLT :: Int -> (Bool, ())
slowFibWriterLT = L.runWriter . slowFib

slowFibWriterCT :: Int -> (Bool, ())
slowFibWriterCT = C.runWriter . slowFib

slowFibWriterST :: Int -> (Bool, ())
slowFibWriterST = S.runWriter . slowFib

slowFibRWSST :: Int -> (Bool, (), ())
slowFibRWSST x = S.runRWS (slowFib x) () ()

slowFibRWSLT :: Int -> (Bool, (), ())
slowFibRWSLT x = L.runRWS (slowFib x) () ()

slowFibRWSCT :: Int -> (Bool, (), ())
slowFibRWSCT x = C.runRWS (slowFib x) () ()

slowFibSelectT :: Int -> Bool
slowFibSelectT = (`runSelect` const ()) . slowFib

slowFibAccumT :: Int -> (Bool, ())
slowFibAccumT = (`runAccum` ()) . slowFib

slowFibEff :: Int -> Bool
slowFibEff = E.run . slowFib

slowFibEffSkeleton :: Int -> Bool
slowFibEffSkeleton = Skeleton.leaveEff . slowFib

slowFibRIO :: Int -> IO Bool
slowFibRIO = runRIO () . slowFib

{-
strictFibIdentity :: Int -> Bool
strictFibIdentity = runIdentity . strictFib

strictFibIO :: Int -> IO Bool
strictFibIO = strictFib

strictFibMaybe :: Int -> Maybe Bool
strictFibMaybe = strictFib

strictFibList :: Int -> [Bool]
strictFibList = strictFib

strictFibReader :: Int -> Bool
strictFibReader = flip strictFib ()

strictFibMaybeT :: Int -> Maybe Bool
strictFibMaybeT = runIdentity . runMaybeT . strictFib

strictFibEither :: Int -> Either () Bool
strictFibEither = strictFib

strictFibExceptT :: Int -> Either () Bool
strictFibExceptT = runExcept . strictFib

strictFibStateST :: Int -> (Bool, ())
strictFibStateST = (`S.runState` ()) . strictFib

strictFibStateLT :: Int -> (Bool, ())
strictFibStateLT = (`L.runState` ()) . strictFib

strictFibReaderT :: Int -> Bool
strictFibReaderT = (`runReader` ()) . strictFib

strictFibContT :: Int -> Bool
strictFibContT = (`runCont` id) . strictFib

strictFibWriterLT :: Int -> (Bool, ())
strictFibWriterLT = L.runWriter . strictFib

strictFibWriterCT :: Int -> (Bool, ())
strictFibWriterCT = C.runWriter . strictFib

strictFibWriterST :: Int -> (Bool, ())
strictFibWriterST = S.runWriter . strictFib

strictFibRWSST :: Int -> (Bool, (), ())
strictFibRWSST x = S.runRWS (strictFib x) () ()

strictFibRWSLT :: Int -> (Bool, (), ())
strictFibRWSLT x = L.runRWS (strictFib x) () ()

strictFibRWSCT :: Int -> (Bool, (), ())
strictFibRWSCT x = C.runRWS (strictFib x) () ()

strictFibEff :: Int -> Bool
strictFibEff = E.run . strictFib

strictFibEffSkeleton :: Int -> Bool
strictFibEffSkeleton = Skeleton.leaveEff . strictFib

strictFibRIO :: Int -> IO Bool
strictFibRIO = runRIO () . strictFib
-}