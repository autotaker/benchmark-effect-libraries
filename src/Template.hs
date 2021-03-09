{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Template where

import Control.Eff
import Control.Eff.Extend
import Control.Eff.Trace
import Control.Monad (forM)
import Data.Function (fix)
import Language.Haskell.TH hiding (lift)
import qualified System.IO as Sys

decl :: String -> Q [Dec]
decl methodName = do
  {- data M a where
   -   M :: Int -> M ()
   -}
  datD <- do
    int <- [t|Int|]
    unit <- [t|()|]
    let con = GadtC [m] [(Bang NoSourceUnpackedness SourceStrict, int)] (AppT (ConT m) unit)
    pure $ DataD [] m [PlainTV (mkName "a")] Nothing [con] []
  handleD <-
    [d|
      instance Lifted IO r => Handle $tyM ($tyM ': r) a (Sys.Handle -> Eff r a) where
        handle step q sreq h =
          case sreq of
            $patM -> {- lift (Sys.hPrint h x) >> -} step (q ^$ ()) h
      |]
  --let runPragmaD = PragmaD $ InlineP runM NoInline FunLike AllPhases
  -- runM :: forall r a.Lifted IO r => Sys.Handle -> Eff ($tyM ': r) a -> Eff r a
  runSigD <- sigD runM [t|forall r a. Lifted IO r => Sys.Handle -> Eff ($tyM ': r) a -> Eff r a|]
  -- runM h a = fix (handle_relay pure) a h
  runD <- funD runM [clause [[p|h|], [p|a|]] (NormalB <$> [|fix (handle_relay (\a _ -> pure a)) a h|]) []]
  pure $ datD : {-runPragmaD :-} runSigD : runD : handleD
  where
    m = mkName methodName
    tyM = pure $ ConT m
    runM = mkName ("run" ++ methodName)
    patM = pure $ ConP m [VarP (mkName "x")]

declMany :: Int -> Q [Dec]
declMany n = do
  l <- forM [1 .. n] $ \i -> decl ("M" ++ show i)
  pure $ concat l