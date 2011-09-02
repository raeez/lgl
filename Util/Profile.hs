module Main where
import Data.Graph.Linear.Graph
import Data.Graph.Linear.Query.SCC
import Data.Graph.Linear.Query.BCC
import Util.Dense(genDenseGraph)
import Util.Sparse(genSparseGraph)
import Util.Random(genRandomGraph)

main = print $ biConnectedComp $ genRandomGraph 500
