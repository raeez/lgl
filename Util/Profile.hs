module Main where
import Data.Graph.Linear.Graph
import Data.Graph.Linear.Query.SCC
import Dense(genDenseGraph)
import Sparse(genSparseGraph)
import Random(genRandomGraph)

main = print $ fst $ scc $ mkGraph $ map nodeConstructor $ genRandomGraph 1000
