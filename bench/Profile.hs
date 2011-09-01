module Main where
-- import Frozen.GraphSCC
import Data.Graph.Linear.Query.SCC
import Dense(genDenseGraph)
import Sparse(genSparseGraph)
import Random(genRandomGraph)
import qualified Frozen.ContainersGraph


main = print $ stronglyConnComp $ genDenseGraph 1000

instance Show a => Show (Frozen.ContainersGraph.SCC a) where
  show (Frozen.ContainersGraph.CyclicSCC v)  = show v
  show (Frozen.ContainersGraph.AcyclicSCC v) = show v
