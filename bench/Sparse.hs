module Sparse (sparseSuite) where
import Criterion.Main
import Data.List
import Util

-- | filters the sublist formed by taking every n'th element, after some shift s
shiftedCycle :: Int -> Int -> [a] -> [a]
shiftedCycle s 0 xs = shiftedCycle s 1 xs
shiftedCycle s n xs = map snd $ filter selectShiftedInterval $ zip [1..] xs
  where
    selectShiftedInterval = \(i, x) -> (i + s) `mod` n == 0

genSparseEdges :: Vertices -> EdgedGraph
genSparseEdges vs =
    fst $ foldl' (\(nodes, s:ss) (node, key) -> let keys = shiftedCycle s s [1..length vs]
                                               in ((node, key, keys):nodes, ss))
                 ([], cycle)
                 vs
  where
    cycle = [1..length vs] ++ cycle

genSparseGraph :: EdgeConstructor
genSparseGraph = \n -> genSparseEdges $ genVertices n

sparseSuite :: [([Benchmark], [Benchmark])]
sparseSuite = map (benchSuite "sparse" genSparseGraph) [1000, 5000, 10000, 20000]
