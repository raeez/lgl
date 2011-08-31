{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Linear.Graph
-- Copyright   :  
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Data.Graph.Linear.Basic implements various representation and convenience
-- constructors for linear time and space graph algorithms.
--
-----------------------------------------------------------------------------
--
module Data.Graph.Linear.Graph
(
    Bounds, Vertex, Edge
  , Node(..)
  , GraphRepresentation(..)
  , Graph(..)
  , graphFromEdgedVertices, nodeConstructor
) 
where

#ifdef USE_MAPS
import Data.Graph.Linear.Representation.Map
#endif

#ifdef USE_VECTOR
import Data.Graph.Linear.Representation.Vector as Rep
#endif

#ifdef USE_ARRAY
import Data.Graph.Linear.Representation.Array as Rep
#endif

import Data.Maybe(mapMaybe)
import Data.List

-------------------------------------------------------------------------------
-- Internal Graph Representation

-- | A single Vertex represented in an InternalGraph.
type Vertex        = Int

-- | A single edge represented in an InternalGraph
type Edge          = (Vertex, Vertex)

-- | An internal-only adjacency list mapping from Vertices to neighboring vertices.
type InternalGraph = Rep.Mapping [Vertex]

-------------------------------------------------------------------------------
-- External Graph Representation

-- | A single 'Node' represents an addressable vertex within a 'Graph'
data Node payload label = Node
  { payload    :: payload
  , label      :: !label
  , successors :: ![label]
  }

instance Eq l => Eq (Node p l) where
  (==) (Node _ l1 _) (Node _ l2 _) = l1 == l2 
  (/=) (Node _ l1 _) (Node _ l2 _) = l1 /= l2

instance Ord l => Ord (Node p l) where
  compare (Node _ l1 _) (Node _ l2 _)
          | l1 == l2   = EQ
          | l1 <= l2   = LT
          | otherwise = GT
-------------------------------------------------------------------------------
-- Constructors

-- | Given an arbitrary representation we can construct a graph.
class Ord node => GraphRepresentation node where
  data Graph node :: *

  empty      :: Graph node
  mkGraph    :: [node]     -> Graph node
  bounds     :: Graph node -> Bounds

  vertices   :: Graph node -> [Vertex]
  edges      :: Graph node -> [Edge]

  adjacentTo :: Graph node -> Vertex -> [Vertex]

instance Ord l => GraphRepresentation (Node p l) where 
  data Graph (Node p l)    = Graph 
    { grAdjacencyList :: {-# UNPACK #-} !InternalGraph
    , grVertexMap     :: Vertex -> Node p l
    , grNodeMap       :: Node p l -> Maybe Vertex
    }

  empty                    = Graph Rep.empty (error "emptyGraph") (const Nothing)
  mkGraph nodes            = Graph intgraph vertex_fn (label_vertex . label)
    where
      (vertex_fn, label_vertex, numbered_nodes) = reduceNodesIntoVertices nodes label

      reduceNodesIntoVertices nodes label_extractor = ((!) vertex_map, label_vertex, numbered_nodes)
        where
          max_v           = length nodes - 1
          sorted_nodes    = let n1 `le` n2 = (label_extractor n1 `compare` label_extractor n2)
                            in sortBy le nodes
          numbered_nodes  = [0..] `zip` sorted_nodes

          key_map         = Rep.fromList [(v, label_extractor node) | (v, node) <- numbered_nodes]
          vertex_map      = Rep.fromList numbered_nodes

          --label_vertex :: label -> Maybe Vertex
          -- returns Nothing for non-interesting vertices
          label_vertex k = find 0 max_v
            where
              find a b | a > b     = Nothing
                       | otherwise = let mid = (a + b) `div` 2
                                     in case compare k (key_map ! mid) of
                                          LT -> find a (mid - 1)
                                          EQ -> Just mid
                                          GT -> find (mid + 1) b

      intgraph = Rep.fromList [(v, mapMaybe label_vertex ks) | (v, Node _ _ ks) <- numbered_nodes]

  vertices   (Graph g _ _) = domain g
  edges      (Graph g _ _) = [ (v, w) | v <- domain g, w <- g ! v ]
  bounds     (Graph g _ _) = domainBounds g

  adjacentTo (Graph g _ _) = (!) g

graphFromEdgedVertices :: Ord label => [(payload, label, [label])] -> Graph (Node payload label)
graphFromEdgedVertices = mkGraph . map nodeConstructor

nodeConstructor :: Ord l => (p, l, [l]) -> Node p l
nodeConstructor = \(p, l, ls) -> Node p l ls
