module Util where
import Criterion.Main
import Control.DeepSeq
import Frozen.ContainersGraph (SCC(..))
import qualified Frozen.Digraph as G
import qualified Frozen.ContainersGraph as C
import qualified Frozen.GraphSCC as S


type Vertices         = [(Char, Int)]
type EdgeConstructor  = Int -> EdgedGraph
type EdgedGraph       = [G.Node Int Char]
type Size             = Int

-- | generate a suite of bench marks given a graphSize and graphType
sccBenchSuite :: String -> EdgeConstructor -> Size -> [Benchmark]
sccBenchSuite name graphType n =
    [ bench ("Digraph [" ++ name ++  ":" ++ show n ++ "]")    $ nf G.stronglyConnCompFromEdgedVertices inputGraph
    , bench ("Containers [" ++ name ++ ":" ++  show n ++ "]") $ nf C.stronglyConnComp inputGraph
    , bench ("Graph.SCC [" ++ name ++ ":" ++ show n ++ "]")   $ nf S.stronglyConnComp inputGraph
    ]
  where
    inputGraph = graphType n

-----------------------------------------------------
-- instances for forcing head normal form evaluation
instance NFData v => NFData (G.SCC v) where
    rnf (G.AcyclicSCC v) = rnf v
    rnf (G.CyclicSCC v) = rnf v

instance NFData v => NFData (C.SCC v) where
    rnf (C.AcyclicSCC v) = rnf v
    rnf (C.CyclicSCC v)  = rnf v
