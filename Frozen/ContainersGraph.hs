{-# LANGUAGE RankNTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.ContainersGraph
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- A version of the graph algorithms described in:
--
--   /Lazy Depth-First Search and Linear Graph Algorithms in Haskell/,
--   by David King and John Launchbury.
--
-----------------------------------------------------------------------------

module Frozen.ContainersGraph(

	-- * External interface

	-- At present the only one with a "nice" external interface
	stronglyConnComp, stronglyConnCompR, SCC(..), flattenSCC, flattenSCCs,

	-- * Graphs

	Graph, Table, Bounds, Edge, Vertex,

	-- ** Building graphs

	graphFromEdges, graphFromEdges', buildG, transposeG,
	-- reverseE,

	-- ** Graph properties

	vertices, edges,
	outdegree, indegree,

	-- * Algorithms

	dfs, dff,
	topSort,
	components,
	scc,
	bcc,
	-- tree, back, cross, forward,
	reachable, path,

	module Data.Tree

    ) where

-- Extensions
import Control.Monad.ST
import Data.Array.ST (STArray, newArray, readArray, writeArray)
import Data.Tree (Tree(Node), Forest)

-- std interfaces
import Data.Maybe
import Data.Array
import Data.List

import Prelude

-------------------------------------------------------------------------
--									-
--	External interface
--									-
-------------------------------------------------------------------------

-- | Strongly connected component.
data SCC vertex = AcyclicSCC vertex	-- ^ A single vertex that is not
					-- in any cycle.
	        | CyclicSCC  [vertex]	-- ^ A maximal set of mutually
					-- reachable vertices.

-- | The vertices of a list of strongly connected components.
flattenSCCs :: [SCC a] -> [a]
flattenSCCs = concatMap flattenSCC

-- | The vertices of a strongly connected component.
flattenSCC :: SCC vertex -> [vertex]
flattenSCC (AcyclicSCC v) = [v]
flattenSCC (CyclicSCC vs) = vs

-- | The strongly connected components of a directed graph, topologically
-- sorted.
stronglyConnComp
	:: Ord key
	=> [(node, key, [key])]
		-- ^ The graph: a list of nodes uniquely identified by keys,
		-- with a list of keys of nodes this node has edges to.
		-- The out-list may contain keys that don't correspond to
		-- nodes of the graph; such edges are ignored.
	-> [SCC node]

stronglyConnComp edges0
  = map get_node (stronglyConnCompR edges0)
  where
    get_node (AcyclicSCC (n, _, _)) = AcyclicSCC n
    get_node (CyclicSCC triples)     = CyclicSCC [n | (n,_,_) <- triples]

-- | The strongly connected components of a directed graph, topologically
-- sorted.  The function is the same as 'stronglyConnComp', except that
-- all the information about each node retained.
-- This interface is used when you expect to apply 'SCC' to
-- (some of) the result of 'SCC', so you don't want to lose the
-- dependency information.
stronglyConnCompR
	:: Ord key
	=> [(node, key, [key])]
		-- ^ The graph: a list of nodes uniquely identified by keys,
		-- with a list of keys of nodes this node has edges to.
		-- The out-list may contain keys that don't correspond to
		-- nodes of the graph; such edges are ignored.
	-> [SCC (node, key, [key])]	-- ^ Topologically sorted

stronglyConnCompR [] = []  -- added to avoid creating empty array in graphFromEdges -- SOF
stronglyConnCompR edges0
  = map decode forest
  where
    (graph, vertex_fn,_) = graphFromEdges edges0
    forest	       = scc graph
    decode (Node v [])
           | mentions_itself v = CyclicSCC [vertex_fn v]
		       | otherwise	       = AcyclicSCC (vertex_fn v)
    decode other = CyclicSCC (dec other [])
		 where
		   dec (Node v ts) vs = vertex_fn v : foldr dec vs ts
    mentions_itself v = v `elem` (graph ! v)

-------------------------------------------------------------------------
--									-
--	Graphs
--									-
-------------------------------------------------------------------------

-- | Abstract representation of vertices.
type Vertex  = Int
-- | Table indexed by a contiguous set of vertices.
type Table a = Array Vertex a
-- | Adjacency list representation of a graph, mapping each vertex to its
-- list of successors.
type Graph   = Table [Vertex]
-- | The bounds of a 'Table'.
type Bounds  = (Vertex, Vertex)
-- | An edge from the first vertex to the second.
type Edge    = (Vertex, Vertex)

-- | All vertices of a graph.
vertices :: Graph -> [Vertex]
vertices  = indices

-- | All edges of a graph.
edges    :: Graph -> [Edge]
edges g   = [ (v, w) | v <- vertices g, w <- g!v ]

mapT    :: (Vertex -> a -> b) -> Table a -> Table b
mapT f t = array (bounds t) [ (,) v (f v (t!v)) | v <- indices t ]

-- | Build a graph from a list of edges.
buildG :: Bounds -> [Edge] -> Graph
buildG bounds0 edges0 = accumArray (flip (:)) [] bounds0 edges0

-- | The graph obtained by reversing all edges.
transposeG  :: Graph -> Graph
transposeG g = buildG (bounds g) (reverseE g)

reverseE    :: Graph -> [Edge]
reverseE g   = [ (w, v) | (v, w) <- edges g ]

-- | A table of the count of edges from each node.
outdegree :: Graph -> Table Int
outdegree  = mapT numEdges
             where numEdges _ ws = length ws

-- | A table of the count of edges into each node.
indegree :: Graph -> Table Int
indegree  = outdegree . transposeG

-- | Identical to 'graphFromEdges', except that the return value
-- does not include the function which maps keys to vertices.  This
-- version of 'graphFromEdges' is for backwards compatibility.
graphFromEdges'
	:: Ord key
	=> [(node, key, [key])]
	-> (Graph, Vertex -> (node, key, [key]))
graphFromEdges' x = (a,b) where
    (a,b,_) = graphFromEdges x

-- | Build a graph from a list of nodes uniquely identified by keys,
-- with a list of keys of nodes this node should have edges to.
-- The out-list may contain keys that don't correspond to
-- nodes of the graph; they are ignored.
graphFromEdges
	:: Ord key
	=> [(node, key, [key])]
	-> (Graph, Vertex -> (node, key, [key]), key -> Maybe Vertex)
graphFromEdges edges0
  = (graph, \v -> vertex_map ! v, key_vertex)
  where
    max_v      	    = length edges0 - 1
    bounds0         = (0,max_v) :: (Vertex, Vertex)
    sorted_edges    = sortBy lt edges0
    edges1	        = zipWith (,) [0..] sorted_edges

    graph	          = array bounds0 [(,) v (mapMaybe key_vertex ks) | (,) v (_,    _, ks) <- edges1]
    key_map	        = array bounds0 [(,) v k			   | (,) v (_,    k, _ ) <- edges1]
    vertex_map	    = array bounds0 edges1

    (_,k1,_) `lt` (_,k2,_) = k1 `compare` k2

    -- key_vertex :: key -> Maybe Vertex
    -- 	returns Nothing for non-interesting vertices
    key_vertex k   = findVertex 0 max_v
		   where
		     findVertex a b | a > b
			      = Nothing
		     findVertex a b = case compare k (key_map ! mid) of
				   LT -> findVertex a (mid-1)
				   EQ -> Just mid
				   GT -> findVertex (mid+1) b
			      where
			 	mid = (a + b) `div` 2

-------------------------------------------------------------------------
--									-
--	Depth first search
--									-
-------------------------------------------------------------------------

-- | A spanning forest of the graph, obtained from a depth-first search of
-- the graph starting from each vertex in an unspecified order.
dff          :: Graph -> Forest Vertex
dff g         = dfs g (vertices g)

-- | A spanning forest of the part of the graph reachable from the listed
-- vertices, obtained from a depth-first search of the graph starting at
-- each of the listed vertices in order.
dfs          :: Graph -> [Vertex] -> Forest Vertex
dfs g vs      = prune (bounds g) (map (generate g) vs)

generate     :: Graph -> Vertex -> Tree Vertex
generate g v  = Node v (map (generate g) (g!v))

prune        :: Bounds -> Forest Vertex -> Forest Vertex
prune bnds ts = run bnds (chop ts)

chop         :: Forest Vertex -> SetM s (Forest Vertex)
chop []       = return []
chop (Node v ts : us)
              = do
                visited <- contains v
                if visited then
                  chop us
                 else do
                  include v
                  as <- chop ts
                  bs <- chop us
                  return (Node v as : bs)

-- A monad holding a set of vertices visited so far.

newtype SetM s a = SetM { runSetM :: STArray s Vertex Bool -> ST s a }

instance Monad (SetM s) where
    return x     = SetM $ const (return x)
    SetM v >>= f = SetM $ \ s -> do { x <- v s; runSetM (f x) s }

run          :: Bounds -> (forall s. SetM s a) -> a
run bnds act  = runST (newArray bnds False >>= runSetM act)

contains     :: Vertex -> SetM s Bool
contains v    = SetM $ \ m -> readArray m v

include      :: Vertex -> SetM s ()
include v     = SetM $ \ m -> writeArray m v True

-------------------------------------------------------------------------
--									-
--	Algorithms
--									-
-------------------------------------------------------------------------

------------------------------------------------------------
-- Algorithm 1: depth first search numbering
------------------------------------------------------------

preorder            :: Tree a -> [a]
preorder (Node a ts) = a : preorderF ts

preorderF           :: Forest a -> [a]
preorderF ts         = concat (map preorder ts)

tabulate        :: Bounds -> [Vertex] -> Table Int
tabulate bnds vs = array bnds (zipWith (,) vs [1..])

preArr          :: Bounds -> Forest Vertex -> Table Int
preArr bnds      = tabulate bnds . preorderF

------------------------------------------------------------
-- Algorithm 2: topological sorting
------------------------------------------------------------

postorder :: Tree a -> [a]
postorder (Node a ts) = postorderF ts ++ [a]

postorderF   :: Forest a -> [a]
postorderF ts = concat (map postorder ts)

postOrd      :: Graph -> [Vertex]
postOrd       = postorderF . dff

-- | A topological sort of the graph.
-- The order is partially specified by the condition that a vertex /i/
-- precedes /j/ whenever /j/ is reachable from /i/ but not vice versa.
topSort      :: Graph -> [Vertex]
topSort       = reverse . postOrd

------------------------------------------------------------
-- Algorithm 3: connected components
------------------------------------------------------------

-- | The connected components of a graph.
-- Two vertices are connected if there is a path between them, traversing
-- edges in either direction.
components   :: Graph -> Forest Vertex
components    = dff . undirected

undirected   :: Graph -> Graph
undirected g  = buildG (bounds g) (edges g ++ reverseE g)

-- Algorithm 4: strongly connected components

-- | The strongly connected components of a graph.
scc  :: Graph -> Forest Vertex
scc g = dfs g (reverse (postOrd (transposeG g)))

------------------------------------------------------------
-- Algorithm 5: Classifying edges
------------------------------------------------------------

{-
XXX unused code

tree              :: Bounds -> Forest Vertex -> Graph
tree bnds ts       = buildG bnds (concat (map flat ts))
 where flat (Node v ts') = [ (v, w) | Node w _us <- ts' ]
                        ++ concat (map flat ts')

back              :: Graph -> Table Int -> Graph
back g post        = mapT select g
 where select v ws = [ w | w <- ws, post!v < post!w ]

cross             :: Graph -> Table Int -> Table Int -> Graph
cross g pre post   = mapT select g
 where select v ws = [ w | w <- ws, post!v > post!w, pre!v > pre!w ]

forward           :: Graph -> Graph -> Table Int -> Graph
forward g tree' pre = mapT select g
 where select v ws = [ w | w <- ws, pre!v < pre!w ] \\ tree' ! v
-}

------------------------------------------------------------
-- Algorithm 6: Finding reachable vertices
------------------------------------------------------------

-- | A list of vertices reachable from a given vertex.
reachable    :: Graph -> Vertex -> [Vertex]
reachable g v = preorderF (dfs g [v])

-- | Is the second vertex reachable from the first?
path         :: Graph -> Vertex -> Vertex -> Bool
path g v w    = w `elem` (reachable g v)

------------------------------------------------------------
-- Algorithm 7: Biconnected components
------------------------------------------------------------

-- | The biconnected components of a graph.
-- An undirected graph is biconnected if the deletion of any vertex
-- leaves it connected.
bcc :: Graph -> Forest [Vertex]
bcc g = (concat . map bicomps . map (do_label g dnum)) forest
 where forest = dff g
       dnum   = preArr (bounds g) forest

do_label :: Graph -> Table Int -> Tree Vertex -> Tree (Vertex,Int,Int)
do_label g dnum (Node v ts) = Node (v,dnum!v,lv) us
 where us = map (do_label g dnum) ts
       lv = minimum ([dnum!v] ++ [dnum!w | w <- g!v]
                     ++ [lu | Node (_,_,lu) _ <- us])

bicomps :: Tree (Vertex,Int,Int) -> Forest [Vertex]
bicomps (Node (v,_,_) ts)
      = [ Node (v:vs) us | (_,Node vs us) <- map collect ts]

collect :: Tree (Vertex,Int,Int) -> (Int, Tree [Vertex])
collect (Node (v,dv,lv) ts) = (lv, Node (v:vs) cs)
 where collected = map collect ts
       vs = concat [ ws | (lw, Node ws _) <- collected, lw<dv]
       cs = concat [ if lw<dv then us else [Node (v:ws) us]
                        | (lw, Node ws us) <- collected ]
