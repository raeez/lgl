module Dense (denseSuite) where
import Criterion.Main
import Util

genVertices :: Int -> Vertices
genVertices n = zip ['A'..] [1..n]

genMaximallyConnectedEdges :: Vertices -> EdgedGraph
genMaximallyConnectedEdges vs = map (\(node, key) -> (node, key, map snd vs)) vs

genDenseGraph :: EdgeConstructor
genDenseGraph n = genMaximallyConnectedEdges $ genVertices n

denseSuite :: [Benchmark]
denseSuite = concatMap (sccBenchSuite "dense" genDenseGraph) [10, 50, 100, 500]
