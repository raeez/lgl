{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Linear.Graph
-- Copyright   :  
-- License     :  BSD-style (see the file libraries/base/LICENSE)
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
  Vertex,
  Node(..),
  GraphRepresentation(..)
) 
where

import Data.Graph.Linear.Representation.Array as Rep -- Mapping, mkMapping
import Data.Maybe(mapMaybe)
import Data.List
-- import Data.Graph.Linear.Representation.Vector
-- import Data.Graph.Linear.Representation.HashMap
-- import Data.Graph.Linear.Representation.Map

-------------------------------------------------------------------------------
-- Internal Graph Representation

type Bounds        = (Vertex, Vertex)
type Vertex        = Int
type Edge          = (Vertex, Vertex)
type InternalGraph = Rep.Mapping Vertex [Vertex]

-------------------------------------------------------------------------------
-- External Graph Representation

-- | A single 'Node' represents an addressable vertex within a 'Graph'
data Node payload label = Node payload label [label]

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
  mkBack     :: [node]     -> (Vertex -> node)
  bounds     :: Graph node -> Bounds

  vertices   :: Graph node -> [Vertex]
  edges      :: Graph node -> [Edge]

  adjacentTo :: Graph node -> Vertex -> [Vertex]

instance Ord l => GraphRepresentation (Node p l) where 
  data Graph (Node p l) = Graph InternalGraph (Vertex -> Node p l) (Node p l -> Maybe Vertex)
  empty                  = Graph Rep.empty (error "emptyGraph") (const Nothing)
  mkGraph edged_vertices = Graph graph vertex_fn (key_vertex . label_extractor)
    where
      label_extractor (Node _ l _) = l
      ke (_, k, _) = k
      tupled_vertices = map (\(Node p l ls) -> (p, l, ls)) edged_vertices
      (bounds, vertex_fn, key_vertex, numbered_nodes) = reduceNodesIntoVertices tupled_vertices ke
      graph = mkMap bounds [(v, mapMaybe key_vertex ks) | (v, (_, _, ks)) <- numbered_nodes]

  mkBack edged_vertices = vertex_fn
    where
      label_extractor (Node _ l _) = l
      ke (_, k, _) = k
      tupled_vertices = map (\(Node p l ls) -> (p, l, ls)) edged_vertices
      (bounds, vertex_fn, key_vertex, numbered_nodes) = reduceNodesIntoVertices tupled_vertices ke

  vertices   (Graph g _ _) = domain g
  edges      (Graph g _ _) = [ (v, w) | v <- domain g, w <- g ! v ]
  bounds     (Graph g _ _) = domainBounds g

  adjacentTo (Graph g _ _) = (!) g

{-reduceNodesIntoVertices 
        :: Ord key 
        => [node] 
        -> (node -> key) 
        -> (Bounds, Vertex -> node, key -> Maybe Vertex, [(Int, node)])-}
reduceNodesIntoVertices nodes key_extractor = (bounds, (!) vertex_map, key_vertex, numbered_nodes)
  where
    max_v           = length nodes - 1
    bounds          = (0, max_v) :: (Vertex, Vertex)

    (_,k1,_) `lt` (_,k2,_) = k1 `compare` k2
    sorted_nodes    = let n1 `le` n2 = (key_extractor n1 `compare` key_extractor n2) /= GT
                      in sortBy lt nodes
    numbered_nodes  = zipWith (,) [0..] sorted_nodes

    key_map         = mkMap bounds [(i, key_extractor node) | (i, node) <- numbered_nodes]
    vertex_map      = mkMap bounds $ map (\(i, (p, l, ls)) -> (i, Node p l ls)) numbered_nodes

    --key_vertex :: key -> Maybe Vertex
    -- returns Nothing for non-interesting vertices
    key_vertex k = find 0 max_v
      where
        find a b | a > b = Nothing
                 | otherwise = let mid = (a + b) `div` 2
                               in case compare k (key_map ! mid) of
                                    LT -> find a (mid - 1)
                                    EQ -> Just mid
                                    GT -> find (mid + 1) b
{-
graphFromVerticesAndAdjacency
        :: Ord key
        => [(node, key)]
        -> [(key, key)]  -- First component is source vertex key, 
                         -- second is target vertex key (thing depended on)
                         -- Unlike the other interface I insist they correspond to
                         -- actual vertices because the alternative hides bugs. I can't
                         -- do the same thing for the other one for backcompat reasons.

                         -- TODO what backcompat reasons?
        -> Graph (node, key)
graphFromVerticesAndAdjacency []       _     = emptyGraph
graphFromVerticesAndAdjacency vertices edges = Graph graph vertex_node (key_vertex . key_extractor)
  where key_extractor = snd
        (bounds, vertex_node, key_vertex, _) = reduceNodesIntoVertices vertices key_extractor
        key_vertex_pair (a, b) = (expectJust "graphFromVerticesAndAdjacency" $ key_vertex a, 
                                  expectJust "graphFromVerticesAndAdjacency" $ key_vertex b)
        reduced_edges = map key_vertex_pair edges
        graph = buildG bounds reduced_edges

-}
{-
graphFromEdgedVertices
        :: Ord key
        => [Node key payload]           -- The graph; its ok for the
                                        -- out-list to contain keys which arent
                                        -- a vertex key, they are ignored
        -> Graph (Node key payload)
graphFromEdgedVertices []             = emptyGraph
graphFromEdgedVertices edged_vertices = Graph graph vertex_fn (key_vertex . key_extractor)
  where key_extractor (_, k, _) = k
        (bounds, vertex_fn, key_vertex, numbered_nodes) = reduceNodesIntoVertices edged_vertices key_extractor
        graph = array bounds [(v, mapMaybe key_vertex ks) | (v, (_, _, ks)) <- numbered_nodes]
-}
