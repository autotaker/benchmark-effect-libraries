{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Reader where

import qualified Control.Eff as EE
import qualified Control.Eff.Reader.Strict as EE
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import qualified Control.Monad.Trans.Reader as ReaderT
import qualified Data.Extensible.Effect as Skeleton
import Data.Functor.Identity (Identity (runIdentity))
import Data.Proxy (Proxy (Proxy))
import qualified RIO

type Ask m r = m r

type Local m r = forall a. (r -> r) -> m a -> m a

fib :: Monad m => Ask m Int -> Local m Int -> m Int
fib ask local = go
  where
    go = do
      n <- ask
      if n <= 1
        then pure n
        else do
          r1 <- local (const (n -1)) go
          r2 <- local (const (n -2)) go
          pure $ r1 + r2

fibReaderT :: Int -> Int
fibReaderT n = runIdentity $ runReaderT (fib ReaderT.ask ReaderT.local) n

fibEEEff :: Int -> Int
fibEEEff n = EE.run $ EE.runReader n (fib EE.ask EE.local)

fibRIO :: Int -> IO Int
fibRIO n = RIO.runRIO n (fib RIO.ask RIO.local)

fibSkeleton :: Int -> Int
fibSkeleton n =
  Skeleton.leaveEff $
    Skeleton.runReaderEff @"R"
      (fib (Skeleton.askEff (Proxy @"R")) (Skeleton.localEff (Proxy @"R")))
      n