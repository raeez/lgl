module Dense (denseSuite) where
import Criterion.Main
import Util

genMaximallyConnectedEdges :: Vertices -> EdgedGraph
genMaximallyConnectedEdges vs = map (\(node, key) -> (node, key, map snd vs)) vs

genDenseGraph :: EdgeConstructor
genDenseGraph = \n -> genMaximallyConnectedEdges $ genVertices n

denseSuite :: [([Benchmark], [Benchmark])]
denseSuite = map (benchSuite "dense" genDenseGraph) [10, 50, 100, 500, 1000, 1500 , 2000, 2500]
