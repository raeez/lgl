module Random (genRandomGraph) where
import Data.List
import System.Random
import Util
import Debug.Trace

generator :: StdGen
generator = mkStdGen 0
-- ^ deterministic: perhaps seed with unsafePerformIO?

genRandomEdges :: RandomGen g => g -> Vertices -> EdgedGraph
genRandomEdges g vs =
    fst $ genRandomEdges' vs
  where
    rs = randomRs (1 :: Int, length vs :: Int) g
    genRandomEdges' =
        foldl' (\(nodes, es:rs) (node, key) -> let (keys, rs') = splitAt (es) rs
                                              in ((node, key, keys):nodes, rs'))
               ([],rs)
    -- ^ folds random edge construction across a tuple of (list of edges, infinite list of random numbers)

genRandomGraph :: EdgeConstructor
genRandomGraph = \n -> genRandomEdges generator $ genVertices n
