import Criterion.Main
import qualified Data.Graph.GHCGraph as G
import qualified Data.Graph.ContainersGraph as C
import qualified Data.Graph.SCC as S
import Data.Graph (SCC(..))
import Data.Graph
import Control.DeepSeq

genVertices :: Int -> [(Char, Int)]
genVertices n = zip ['A'..] [1..n]

genMaximallyConnectedEdges :: [(Char, Int)] -> [G.Node Int Char]
genMaximallyConnectedEdges vs = map (\(node, key) -> (node, key, map snd vs)) vs

genDenseGraph :: Int -> [G.Node Int Char]
genDenseGraph n = genMaximallyConnectedEdges $ genVertices n
                  -- ^ we generate the appropriate graph type for the various interfaces

benchSuite :: Int -> [Benchmark]
benchSuite n =
    [ bench ("Digraph [" ++ show n ++ "]")    $ nf G.stronglyConnCompFromEdgedVertices inputGraph
    , bench ("Containers [" ++ show n ++ "]") $ nf C.stronglyConnComp  inputGraph
    , bench ("Graph.SCC [" ++ show n ++ "]")  $ nf S.stronglyConnComp  inputGraph
    ]
  where
    inputGraph = genDenseGraph n

main :: IO ()
main = defaultMain [
    bgroup "stronglyConnectedComponents" $ concatMap benchSuite [10, 100, 500]
    ]

-----------------------------------------------------
-- instances for forcing head normal form evaluation
instance NFData v => NFData (G.SCC v) where
    rnf (G.AcyclicSCC v) = rnf v
    rnf (G.CyclicSCC v) = rnf v

instance NFData v => NFData (C.SCC v) where
    rnf (C.AcyclicSCC v) = rnf v
    rnf (C.CyclicSCC v)  = rnf v

instance NFData v => NFData (SCC v) where
    rnf (AcyclicSCC v) = rnf v
    rnf (CyclicSCC v) = rnf v

