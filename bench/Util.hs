module Util where
import Criterion.Main
import Control.DeepSeq
import Frozen.ContainersGraph (SCC(..))
import qualified Frozen.Digraph as G
import qualified Frozen.ContainersGraph as C
import qualified Frozen.GraphSCC as S

-- | Common types made synonyms for readability.
type Vertices         = [(Char, Int)]
type EdgeConstructor  = Int -> EdgedGraph
type EdgedGraph       = [G.Node Int Char]
type Size             = Int

-- | generate n vertices
genVertices :: Int -> Vertices
genVertices n = zip ['A'..] [1..n]


numEdges :: EdgedGraph -> Int
numEdges ns  = loop 0 ns
  where
    loop ac []               = ac
    loop ac ((_, _, ks):ns') = let ac' = length ks + ac
                             in seq ac' $ loop ac' ns'
-- | generate a suite of bench marks given a graphSize and graphType
benchSuite :: String -> EdgeConstructor -> Size -> ([Benchmark], [Benchmark])
benchSuite name graphType n = (sccSuite, bccSuite)
  where
    sccSuite = 
      [ bench ("Digraph "    ++ header) $ nf G.stronglyConnCompFromEdgedVertices inputGraph
      , bench ("Containers " ++ header) $ nf C.stronglyConnComp inputGraph
      , bench ("Graph.SCC "  ++ header) $ nf S.stronglyConnComp inputGraph
      ]
    bccSuite =
      [ bench ("Digraph "    ++ header) $ nf (\g -> let g' = G.graphFromEdgedVertices g
                                                   in G.bcc $ G.gr_int_graph g') inputGraph
      , bench ("Containers " ++ header) $ nf (\g -> let (g', _, _) = C.graphFromEdges g
                                                   in C.bcc g') inputGraph
      -- , bench ("Graph.SCC "  ++ header) $ nf S.stronglyConnComp inputGraph
      ]
    inputGraph = graphType n
    edges = numEdges inputGraph
    header = "[" ++ name ++ ":" ++ show n ++ "|" ++ show edges ++ "]"

-----------------------------------------------------
-- instances for forcing head normal form evaluation
instance NFData v => NFData (G.SCC v) where
    rnf (G.AcyclicSCC v) = rnf v
    rnf (G.CyclicSCC v) = rnf v

instance NFData v => NFData (C.SCC v) where
    rnf (C.AcyclicSCC v) = rnf v
    rnf (C.CyclicSCC v)  = rnf v

instance NFData v => NFData (G.Tree v) where
    rnf (G.Node v ts) = seq (rnf v) (seq (rnf ts) ())
