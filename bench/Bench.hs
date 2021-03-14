import Criterion.Main
import Fib

main :: IO ()
main =
  defaultMain
    [ bgroup
        "fib"
        [ bgroup
            ("n=" ++ show n)
            [ bgroup
                "base"
                [ bench "Identity" $ nf fibIdentity n,
                  bench "IO" $ nfIO (fibIO n),
                  bench "Maybe" $ nf fibMaybe n,
                  bench "Either" $ nf fibEither n,
                  bench "List" $ nf fibList n,
                  bench "Reader" $ nf fibReader n
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
                  bench "WriterST" $ nf fibWriterST n,
                  bench "RWSLT" $ nf fibRWSLT n,
                  bench "RWSCT" $ nf fibRWSCT n,
                  bench "RWSST" $ nf fibRWSST n,
                  bench "SelectT" $ nf fibSelectT n,
                  bench "AccumT" $ nf fibAccumT n
                ],
              bgroup
                "others"
                [ bench "Eff" $ nf fibEff n,
                  bench "EffSkeleton" $ nf fibEffSkeleton n,
                  bench "RIO" $ nfIO (fibRIO n)
                ]
            ]
          | n <- [10, 20, 30]
        ],
      bgroup
        "slowFib"
        [ bgroup
            ("n=" ++ show n)
            [ bgroup
                "base"
                [ bench "Identity" $ nf slowFibIdentity n,
                  bench "IO" $ nfIO (slowFibIO n),
                  bench "Maybe" $ nf slowFibMaybe n,
                  bench "Either" $ nf slowFibEither n,
                  bench "List" $ nf slowFibList n,
                  bench "Reader" $ nf slowFibReader n
                ],
              bgroup
                "transformers"
                [ bench "MaybeT" $ nf slowFibMaybeT n,
                  bench "ExceptT" $ nf slowFibExceptT n,
                  bench "StateST" $ nf slowFibStateST n,
                  bench "StateLT" $ nf slowFibStateLT n,
                  bench "ReaderT" $ nf slowFibReaderT n,
                  bench "ContT" $ nf slowFibContT n,
                  bench "WriterLT" $ nf slowFibWriterLT n,
                  bench "WriterCT" $ nf slowFibWriterCT n,
                  bench "WriterST" $ nf slowFibWriterST n,
                  bench "RWSLT" $ nf slowFibRWSLT n,
                  bench "RWSCT" $ nf slowFibRWSCT n,
                  bench "RWSST" $ nf slowFibRWSST n,
                  bench "SelectT" $ nf slowFibSelectT n,
                  bench "AccumT" $ nf slowFibAccumT n
                ],
              bgroup
                "others"
                [ bench "Eff" $ nf slowFibEff n,
                  bench "EffSkeleton" $ nf slowFibEffSkeleton n,
                  bench "RIO" $ nfIO (slowFibRIO n)
                ]
            ]
          | n <- [10, 20, 30]
        ]
    ]