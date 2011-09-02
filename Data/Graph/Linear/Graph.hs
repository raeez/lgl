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

{- #ifdef USE_VECTOR
import Data.Graph.Linear.Representation.Vector as Rep
 #else-}
--import Data.Graph.Linear.Representation.Array as Rep
-- #endif

import qualified Data.Array as A
import Data.Maybe(mapMaybe)
import Data.List(sortBy)

-------------------------------------------------------------------------------
-- Internal Graph Representation

-- | Lower and upper bounds respectively of the domain of the graph's vertices
type Bounds = (Int, Int)

-- | A single Vertex represented in an InternalGraph.
type Vertex = Int

-- | A single edge between any two representations of points in a graph.
type Edge n = (n, n)

-- | An internal-only adjacency list mapping from Vertices to neighboring vertices.
type InternalGraph = A.Array Vertex [Vertex]

-- | Helper function for constructing an InternalGraph from bounds + a list of edges
buildG :: Bounds -> [Edge Vertex] -> InternalGraph
buildG = A.accumArray (flip (:)) []
-------------------------------------------------------------------------------
-- External Graph Representation

-- | A single 'Node' represents an addressable vertex within a 'Graph'. The choice
-- of information stored at the node, as well as the label addressing each node is
-- kept polymorphic.
data Node payload label = Node
  { payload    :: payload  -- ^ The information stored at this point in the graph
  , label      :: !label   -- ^ The label for this node
  , successors :: ![label] -- ^ List of labeled edges rechable from this node.
  }

instance Eq l => Eq (Node p l) where
  (==) (Node _ l1 _) (Node _ l2 _) = l1 == l2 
  (/=) (Node _ l1 _) (Node _ l2 _) = l1 /= l2

instance Ord l => Ord (Node p l) where
  compare (Node _ l1 _) (Node _ l2 _)
          | l1 == l2   = EQ
          | l1 <= l2   = LT
          | otherwise = GT

instance (Show p, Show l) => Show (Node p l) where
  show (Node p l ls) = show p ++ "[" ++  show l ++ "]"
-------------------------------------------------------------------------------
-- Constructors

-- | Given an arbitrary representation of a set of nodes, we can construct a graph.
class Ord node => GraphRepresentation node where
  -- | The Graph data type, polymorphic in the type of node.
  data Graph node :: *

  -- |The empty graph.
  empty            :: Graph node

  -- |Given a list of nodes, we can construct a graph.
  mkGraph          :: [node]     -> Graph node

  -- |List of nodes in this graph.
  nodes            :: Graph node -> [node]

  -- |List of vertices in this graph.
  vertices         :: Graph node -> [Vertex]

  -- |List of edges between nodes in this graph.
  edges            :: Graph node -> [Edge node]

  -- |List of edges between vertices in this graph.
  vedges           :: Graph node -> [Edge Vertex]

  -- |Return the vertices adjacent to a given node in this graph.
  adjacentTo       :: Graph node -> Vertex -> [Vertex]

  -- |Return the upper and lower bounds on the vertex-numbering of this graph's
  -- representation
  bounds           :: Graph node -> Bounds

  -- | The graph obtained by reversing all edges.
  transpose        :: Graph node -> Graph node

  -- | The list of edges with source/destination reversed.
  reverseEdges         :: Graph node -> [Edge node]
  reverseEdges g = [ (w, v) | (v, w) <- edges g ]

  vreverseEdges         :: Graph node -> [Edge Vertex]

  -- | A mapping of the count of edges from each vertex.
  unsafeOutdegree :: Graph node -> (Vertex -> Int)

  -- | A mapping of the count of edges from each node.
  outdegree :: Graph node -> (node -> Maybe Int)

  -- | A mapping of the  count of edges into each vertex.
  unsafeIndegree :: Graph node -> (Vertex -> Int)

  -- | A mapping of the  count of edges into each node.
  indegree :: Graph node -> (node -> Maybe Int)

  -- | Form the undirected version of the given directed graph
  undirected   :: Graph node -> Graph node

instance Ord l => GraphRepresentation (Node p l) where 
  data Graph (Node p l)    = Graph 
    { grAdjacencyList :: {-# UNPACK #-} !InternalGraph
    , grVertexMap     :: Vertex -> Node p l
    , grNodeMap       :: Node p l -> Maybe Vertex
    }

  empty                    = Graph (A.array (1,0) []) (error "emptyGraph") (const Nothing)
  mkGraph nodes            = Graph intgraph (\v -> vertex_map A.! v) (\n -> key_vertex $ label n)
    where
      max_v           = length nodes - 1
      bounds          = (0, max_v)

      (Node _ l1 _) `lt` (Node _ l2 _) = l1 `compare` l2
      sorted_nodes    = sortBy lt nodes

      numbered_nodes  = [0..] `zip` sorted_nodes

      intgraph        = A.array bounds [(v, mapMaybe key_vertex ks) | (v, Node _ _ ks) <- numbered_nodes]
      key_map         = A.array bounds [(v, l) | (v, Node _ l _) <- numbered_nodes]
      vertex_map      = A.array bounds numbered_nodes


      -- key_vertex :: label -> Maybe Vertex
      -- returns Nothing for non-interesting vertices
      key_vertex k    = find 0 max_v
        where
          find a b | a > b     = Nothing
                   | otherwise = let mid = (a + b) `div` 2
                                 in case compare k (key_map A.! mid) of
                                      LT -> find a (mid - 1)
                                      EQ -> Just mid
                                      GT -> find (mid + 1) b

  transpose g@(Graph _ vm nm) = Graph (buildG (bounds g) (vreverseEdges g)) vm nm
  nodes     (Graph g vm _) = map vm (A.indices g)
  vertices  (Graph g vm _) = A.indices g
  edges     (Graph g vm _) = [ (vm v, vm w) | v <- A.indices g, w <- g A.! v ]
  vedges    (Graph g _ _)  = [ (v, w) | v <- A.indices g, w <- g A.! v ]
  adjacentTo (Graph g _ _) = (A.!) g
  bounds    (Graph g _ _)  = A.bounds g

  unsafeOutdegree (Graph g vm _) = \v -> length $ successors $ vm v
  outdegree (Graph g vm nm) = \n -> case nm n of
                                    Nothing -> Nothing
                                    Just v  -> Just $ length $ successors $ vm v
  unsafeIndegree = unsafeIndegree . transpose
  indegree    = outdegree . transpose

  undirected g@(Graph _ vm nm)  = Graph g' vm nm
    where g' = buildG (bounds g) (vedges g ++ vreverseEdges g)

  vreverseEdges g@(Graph _ vm _) = [ (w, v) | (v, w) <- vedges g ]

-- |Helper function for constructing graphs out of lists of tuples.
graphFromEdgedVertices :: Ord label => [(payload, label, [label])] -> Graph (Node payload label)
graphFromEdgedVertices = mkGraph . map nodeConstructor

-- |A helper function for constructing a Node out of a tuple.
nodeConstructor :: Ord l => (p, l, [l]) -> Node p l
nodeConstructor = \(p, l, ls) -> Node p l ls
