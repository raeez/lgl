module Bench where

import Util

import Control.DeepSeq
import Criterion.Main

import qualified Frozen.Digraph as G
import qualified Frozen.ContainersGraph as C
import qualified Frozen.GraphSCC as S
import qualified Data.Graph.Linear as L
import qualified Data.Graph.Linear.Query.SCC as LS
import qualified Data.Graph.Linear.Query.BCC as LB
import qualified Data.Graph.Linear.Graph as LG

-- | generate a suite of bench marks given a graphSize and graphType
benchSuite :: String -> EdgeConstructor -> Size -> ([Benchmark], [Benchmark])
benchSuite name graphType n = (sccSuite, bccSuite)
  where
    sccSuite = 
      [ bench ("Digraph "           ++ header) $ nf G.stronglyConnCompFromEdgedVertices inputGraph
      , bench ("Containers "        ++ header) $ nf C.stronglyConnComp inputGraph
      , bench ("Graph.SCC "         ++ header) $ nf S.stronglyConnComp inputGraph
      , bench ("Data.Graph.Linear " ++ header) $ nf LS.stronglyConnComp inputGraph
      ]
    bccSuite =
      [ bench ("Digraph "    ++ header) $ nf (\g -> let g' = G.graphFromEdgedVertices g
                                                   in G.bcc $ G.gr_int_graph g') inputGraph
      , bench ("Containers " ++ header) $ nf (\g -> let (g', _, _) = C.graphFromEdges g
                                                   in C.bcc g') inputGraph
      , bench ("Data.Graph.Linear " ++ header) $ nf LB.biConnectedComp inputGraph
      ]
    inputGraph = graphType n
    edges = numEdges inputGraph
    header = "[" ++ name ++ ":" ++ show n ++ "|" ++ show edges ++ "]"
-----------------------------------------------------
-- instances for forcing head normal form evaluation
instance NFData v => NFData (G.SCC v) where
    rnf (G.AcyclicSCC v) = rnf v
    rnf (G.CyclicSCC v)  = rnf v

instance NFData v => NFData (C.SCC v) where
    rnf (C.AcyclicSCC v) = rnf v
    rnf (C.CyclicSCC v)  = rnf v

instance NFData v => NFData (G.Tree v) where
    rnf (G.Node v ts) = seq (rnf v) (seq (rnf ts) ())

instance NFData v => NFData(LS.SCC v) where
    rnf (LS.AcyclicSCC v) = rnf v
    rnf (LS.CyclicSCC v)  = rnf v

instance (NFData p, NFData l)  => NFData(LG.Node p l) where
    rnf (LG.Node p l ls) = seq (rnf p) (seq (rnf l) (seq (rnf ls) ()))

instance NFData v => NFData(LB.BCC v) where
    rnf (LB.BCC i v _) = seq (rnf i) (seq (rnf v) ())
