module Main where
import Util.Dense(genDenseGraph)
import Util.Sparse(genSparseGraph)
import Util.Random(genRandomGraph)
import qualified Frozen.GraphSCC as GS
import qualified Frozen.ContainersGraph as CG
import qualified Frozen.Digraph as DG
import qualified Data.Graph.Linear.Graph as LGL
import qualified Data.Graph.Linear.Query.SCC as LGS

import Test.HUnit

--main :: IO ()
main = runTestTT tests

testSCC (h, g) n = [header ~: run ~=? spec]
  where header   = h ++ show n
        spec     = SCCList $ map toLGLSCC $ GS.stronglyConnComp  inputG
        run      = SCCList $ LGS.stronglyConnComp inputG
        inputG   = g n

{-
testBCC (h, g) n = [header ~: spec ~=? run]
  where header   = h ++ show n
        spec     = Frozen.Digraph.bcc inputGraph
        run      = Data.Graph.Linear.Query.BCC.biConnComp inputGraph
        inputG   = g n
-}

randomT n = testSCC ("scc:rand:",   genRandomGraph) n -- ++ testBCC ("bcc:rand:",   genRandomGraph) n
denseT n  = testSCC ("scc:dense:",  genDenseGraph) n  -- ++ testBCC ("bcc:dense:",  genDenseGraph) n
sparseT n = testSCC ("scc:sparse:", genSparseGraph) n -- ++ testBCC ("bcc:sparse:", genSparseGraph) n

tests = test $ concatMap (\n -> randomT n ++ sparseT n ++ denseT n) [1, 10, 20, 30, 40, 50, 100, 1000, 2000]

toLGLSCC (CG.AcyclicSCC v) = LGS.AcyclicSCC v
toLGLSCC (CG.CyclicSCC v)  = LGS.CyclicSCC v

data SCCList v = SCCList [LGS.SCC v] deriving Show

instance Eq a => Eq (LGS.SCC a) where
  (LGS.CyclicSCC v1)  == (LGS.AcyclicSCC v2)  = False
  (LGS.AcyclicSCC v1) == (LGS.CyclicSCC v2)   = False
  (LGS.AcyclicSCC v1) == (LGS.AcyclicSCC v2)  = v1 == v2
  (LGS.CyclicSCC v1)  == (LGS.CyclicSCC v2)   = all (`elem` v2) v1 && all (`elem` v1) v2

instance Eq a => Eq(SCCList a) where
  (SCCList cs1) == (SCCList cs2) = all (`elem` cs2) cs1 && all (`elem` cs1) cs2
