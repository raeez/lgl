module Main where
import Criterion.Main
import Dense
import Sparse
import Random

main :: IO ()
main = defaultMain
    [ -- bgroup "scc" $ concatMap fst denseSuite ++ concatMap fst sparseSuite ++ concatMap fst randomSuite
    bgroup "bcc" $ concatMap snd denseSuite ++ concatMap snd sparseSuite ++ concatMap snd randomSuite
    ]
