module Util.Util where

-- | Common types made synonyms for readability.
type Node key payload = (payload, key, [key])
type Vertices         = [(Int, Int)]
type EdgeConstructor  = Int -> EdgedGraph
type EdgedGraph       = [Node Int Int]
type Size             = Int

-- | generate n vertices
genVertices :: Int -> Vertices
genVertices n = zip [1..n] [1..n]

numEdges :: EdgedGraph -> Int
numEdges ns  = loop 0 ns
  where
    loop ac []               = ac
    loop ac ((_, _, ks):ns') = let ac' = length ks + ac
                             in seq ac' $ loop ac' ns'
