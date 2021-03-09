{-# LANGUAGE LambdaCase #-}

module Fib where

import qualified Control.Eff as E
import Control.Monad.Skeleton (MonadView (Return), Skeleton, deboneBy)
import Control.Monad.Trans.Cont (runCont)
import Control.Monad.Trans.Except (runExcept)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Control.Monad.Trans.Reader (runReader)
import qualified Control.Monad.Trans.State.Lazy as L
import qualified Control.Monad.Trans.State.Strict as S
import qualified Control.Monad.Trans.Writer.CPS as C
import qualified Control.Monad.Trans.Writer.Lazy as L
import qualified Control.Monad.Trans.Writer.Strict as S
import qualified Data.Extensible.Effect as Skeleton
import Data.Functor.Identity (Identity (runIdentity))
import RIO (runRIO)

fib :: Monad m => Int -> m Int
fib = go
  where
    go n
      | n <= 1 = pure n
      | otherwise = do
        r1 <- fib (n -1)
        r2 <- fib (n -2)
        pure $ r1 + r2

{-# NOINLINE slowFib #-}
slowFib :: Monad m => Int -> m Int
slowFib = fib

fibPure :: Int -> Int
fibPure = runIdentity . fib

fibIO :: Int -> IO Int
fibIO = fib

fibMaybe :: Int -> Maybe Int
fibMaybe = fib

fibMaybeT :: Int -> Maybe Int
fibMaybeT = runIdentity . runMaybeT . fib

fibEither :: Int -> Either () Int
fibEither = fib

fibExceptT :: Int -> Either () Int
fibExceptT = runExcept . fib

fibStateST :: Int -> (Int, ())
fibStateST = (`S.runState` ()) . fib

slowFibStateST :: Int -> (Int, ())
slowFibStateST = (`S.runState` ()) . slowFib

fibStateLT :: Int -> (Int, ())
fibStateLT = (`L.runState` ()) . fib

fibReaderT :: Int -> Int
fibReaderT = (`runReader` ()) . fib

fibContT :: Int -> Int
fibContT = (`runCont` id) . fib

fibWriterLT :: Int -> (Int, ())
fibWriterLT = L.runWriter . fib

fibWriterCT :: Int -> (Int, ())
fibWriterCT = C.runWriter . fib

fibWriterST :: Int -> (Int, ())
fibWriterST = S.runWriter . fib

fibEff :: Int -> Int
fibEff = E.run . fib

fibRIO :: Int -> IO Int
fibRIO = runRIO () . fib

data NoEffect a

fibSkeleton :: Int -> Int
fibSkeleton = runSkeleton . fib
  where
    runSkeleton :: Skeleton NoEffect a -> a
    runSkeleton = deboneBy $ \case
      Return a -> a

fibEffSkeleton :: Int -> Int
fibEffSkeleton = Skeleton.leaveEff . fib

slowFibEff :: Int -> Int
slowFibEff = E.run . slowFib

slowFibPure :: Int -> Int
slowFibPure = runIdentity . slowFib

slowFibIO :: Int -> IO Int
slowFibIO = slowFib

slowFibRIO :: Int -> IO Int
slowFibRIO = runRIO () . slowFib