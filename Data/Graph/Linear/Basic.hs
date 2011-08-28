-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Linear.Basic
-- Copyright   :  
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Data.Graph.Linear.Basic implements various convenience constructors for
-- linear time and space graph representations, as well as a set of basic graph
-- algorithms.
--
-----------------------------------------------------------------------------
--
module Data.Graph.Linear.Basic
(
  vertices, edges
)
where

import Data.Graph.Linear.Graph
-- import Data.Graph.Linear.Representation.Array
-- import Data.Graph.Linear.Representation.Vector
-- import Data.Graph.Linear.Representation.HashMap
-- import Data.Graph.Linear.Representation.Map

-------------------------------------------------------------------------------
-- Basic traversals/projections
--
mapT    :: (Vertex -> a -> b) -> Table a -> Table b
mapT f t = array (bounds t) [ (v, f v (t ! v)) | v <- indices t ]

buildG :: Bounds -> [IntEdge] -> IntGraph
buildG bounds edges = accumArray (flip (:)) [] bounds edges

transpose  :: IntGraph -> IntGraph
transpose g = buildG (bounds g) (reverseE g)

reverseE    :: IntGraph -> [IntEdge]
reverseE g   = [ (w, v) | (v, w) <- edges g ]

outdegree :: IntGraph -> Table Int
outdegree  = mapT numEdges
             where numEdges _ ws = length ws

indegree :: IntGraph -> Table Int
indegree  = outdegree . transpose

graphEmpty :: IntGraph -> Bool
graphEmpty g = lo > hi
  where (lo, hi) = bounds g

