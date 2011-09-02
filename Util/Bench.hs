module Main where
import Criterion.Main
import Util.Dense
import Util.Sparse
import Util.Random
import Util.Suite

denseSuite :: [([Benchmark], [Benchmark])]
denseSuite = map (benchSuite "dense" genDenseGraph) [10, 20, 30, 100] -- [10, 50, 100, 500, 1000, 1500 , 2000, 2500]

sparseSuite :: [([Benchmark], [Benchmark])]
sparseSuite = map (benchSuite "sparse" genSparseGraph) [10, 20, 30, 100] -- 1000, 5000, 10000] --, 20000 , 35000, 50000, 80000, 100000] -- , 125000, 150000, 175000, 200000]

randomSuite :: [([Benchmark], [Benchmark])]
randomSuite = map (benchSuite "random" genRandomGraph) [10, 50, 100, 300, 500, 1000, 5000, 10000, 50000, 100000, 150000, 200000]

main :: IO ()
main = defaultMain
    [ -- bgroup "scc" $ concatMap fst denseSuite ++ concatMap fst sparseSuite ++ concatMap fst randomSuite
     bgroup "bcc" $ concatMap snd denseSuite ++ concatMap snd sparseSuite ++ concatMap snd randomSuite
    ]
