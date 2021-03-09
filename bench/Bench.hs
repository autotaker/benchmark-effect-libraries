import Criterion.Main
import Fib
import qualified Reader as R

main :: IO ()
main =
  defaultMain
    [ bgroup
        "fib"
        [ bgroup
            "base"
            [ bench "Identity" $ nf fibPure n,
              bench "IO" $ nfIO (fibIO n),
              bench "Maybe" $ nf fibMaybe n,
              bench "Either" $ nf fibEither n
            ],
          bgroup
            "transformers"
            [ bench "MaybeT" $ nf fibMaybeT n,
              bench "ExceptT" $ nf fibExceptT n,
              bench "StateST" $ nf fibStateST n,
              bench "StateLT" $ nf fibStateLT n,
              bench "ReaderT" $ nf fibReaderT n,
              bench "ContT" $ nf fibContT n,
              bench "WriterLT" $ nf fibWriterLT n,
              bench "WriterCT" $ nf fibWriterCT n,
              bench "WriterST" $ nf fibWriterST n
            ],
          bench "Eff" $ nf fibEff n,
          bench "RIO" $ nfIO (fibRIO n),
          bench "EffSkeleton" $ nf fibEffSkeleton n
        ],
      bgroup
        "slowFib"
        [ bench "Identity" $ nf slowFibPure n,
          bench "IO" $ nfIO (slowFibIO n),
          bench "StateST" $ nf slowFibStateST n,
          bench "Eff" $ nf slowFibEff n,
          bench "RIO" $ nfIO (slowFibRIO n)
        ]
    ]
  where
    n = 10