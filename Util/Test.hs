module Main where
import Util.Dense(genDenseGraph)
import Util.Sparse(genSparseGraph)
import Util.Random(genRandomGraph)
import qualified Frozen.GraphSCC as GS
import qualified Frozen.ContainersGraph as CG
import qualified Frozen.Digraph as DG
import qualified Data.Graph.Linear.Graph as LGL
import qualified Data.Graph.Linear.Query.SCC as LGS
import qualified Data.Graph.Linear.Query.BCC as LGB
import qualified Data.Tree as C

import Data.List(nub, sort)
import Test.HUnit
import Debug.Trace

--main :: IO ()
main = runTestTT tests

consTest g n    = [header ~: run ~=? spec]
  where header  = "constructor:" ++ show n
        spec    = i
        run     = LGL.grAdjacencyList $ LGL.mkGraph $ map LGL.nodeConstructor $ inputG
        (i,_,_) = CG.graphFromEdges inputG
        inputG  = g n

testSCC (h, g) n = consTest g n ++ [header ~: run ~=? spec]
  where header   = h ++ show n
        spec     = SCCList $ map toLGLSCC $ GS.stronglyConnComp  inputG
        run      = SCCList $ LGS.stronglyConnComp inputG
        inputG   = g n

testBCC (h, g) n = consTest g n ++ [header ~: run ~=? spec]
  where header   = h ++ show n
        spec     = BCCList $ concatMap toLGLBCC $ convF $ DG.bcc i'
        run      = BCCList $ map flattenBCC $ LGB.biConnectedComp inputG 
        inputG   = g n
        (i',_,_) = CG.graphFromEdges inputG

randomT n = testSCC ("scc:rand:",   genRandomGraph) n ++ testBCC ("bcc:rand:",   genRandomGraph) n
denseT n  = testSCC ("scc:dense:",  genDenseGraph) n  ++ testBCC ("bcc:dense:",  genDenseGraph) n
sparseT n = testSCC ("scc:sparse:", genSparseGraph) n ++ testBCC ("bcc:sparse:", genSparseGraph) n

tests = test $ concatMap (\n -> randomT n
                            ++ sparseT n
                            ++ denseT n) [1, 10, 20, 30, 40, 50, 100, 1000, 2000]

-- Testing for SCC equality between libraries
toLGLSCC (CG.AcyclicSCC v) = LGS.AcyclicSCC v
toLGLSCC (CG.CyclicSCC v)  = LGS.CyclicSCC v

data SCCList v = SCCList [LGS.SCC v] deriving Show

instance Eq a => Eq(SCCList a) where
  (SCCList cs1) == (SCCList cs2) = all (`elem` cs2) cs1 && all (`elem` cs1) cs2

-- Testing for BCC equality between libraries
data BCCList v = BCCList [FlatBCC v]
data FlatBCC v = FlatBCC [v]

-- toLGLBCC :: C.Tree a -> [FlatBCC a]
-- toLGLBCC t = trace ("\n\n******\ntree: " ++ C.drawTree (fmap show t)) $ map (\vs -> FlatBCC $ sort $ nub vs) (C.flatten t)
toLGLBCC t = map (\vs -> FlatBCC $ sort $ nub vs) (C.flatten t)

convT :: DG.Tree v -> C.Tree v
convT (DG.Node a as) = C.Node a (convF as)

convF :: [DG.Tree v] -> C.Forest v
convF [] = []
convF (t:ts) = convT t:convF ts

-- flattenBCC :: LGB.BCC (LGL.Vertex, LGL.Vertex) -> FlatBCC LGL.Vertex
flattenBCC (LGB.BCC es) =  FlatBCC $ sort $ nub $ es

instance Eq a => Eq(BCCList a) where
  (BCCList cs1) == (BCCList cs2) = all (`elem` cs2) cs1 && all (`elem` cs1) cs2

instance Eq a => Eq(FlatBCC a) where
  (FlatBCC v1) == (FlatBCC v2) = all (`elem` v2) v1 && all (`elem` v1) v2

instance (Ord a, Show a) => Show(BCCList a) where
  show (BCCList cs) = show $ sort cs

instance (Ord a, Show a) => Show(FlatBCC a) where
  show (FlatBCC vs) = show $ sort vs

instance Ord a => Ord(FlatBCC a) where
  (FlatBCC a) `compare` (FlatBCC b) = a `compare` b
