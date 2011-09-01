module Util.Dense (genDenseGraph) where
import Util.Util

genMaximallyConnectedEdges :: Vertices -> EdgedGraph
genMaximallyConnectedEdges vs = map (\(node, key) -> (node, key, map snd vs)) vs

genDenseGraph :: EdgeConstructor
genDenseGraph = \n -> genMaximallyConnectedEdges $ genVertices n
