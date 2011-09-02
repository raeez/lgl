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
    preorder, preorderF, preorderArr
  , postorder, postorderF
  , postOrd
  , topSort
  , components
)
where

import qualified Data.Array as A
import Data.Graph.Linear.Graph
import Data.Graph.Linear.Query.DFS
import qualified Data.Tree as T

-------------------------------------------------------------------------------
-- Basic traversals/projections
--
--
--
------------------------------------------------------------
-- Algorithm 1: depth first search numbering
------------------------------------------------------------
preorder            :: T.Tree a -> [a]
preorder (T.Node a ts) = a : preorderF ts

preorderF           :: T.Forest a -> [a]
preorderF ts         = concatMap preorder ts

preorderArr          :: Bounds -> T.Forest Vertex -> A.Array Int Int
preorderArr bnds f = tabulate bnds (preorderF f)
  where tabulate bnds vs = A.array bnds (vs `zip` [1..])

------------------------------------------------------------
-- Algorithm 2: topological sorting
------------------------------------------------------------

postorder :: T.Tree a -> [a]
postorder (T.Node a ts) = postorderF ts ++ [a]

postorderF   :: T.Forest a -> [a]
postorderF ts = concatMap postorder ts

postOrd      :: GraphRepresentation node => Graph node -> [Vertex]
postOrd       = postorderF . dff

-- | A topological sort of the graph.
-- The order is partially specified by the condition that a vertex /i/
-- precedes /j/ whenever /j/ is reachable from /i/ but not vice versa.
topSort      :: GraphRepresentation node => Graph node -> [Vertex]
topSort       = reverse . postOrd

------------------------------------------------------------
-- Algorithm 3: connected components
------------------------------------------------------------

-- | The connected components of a graph.
-- Two vertices are connected if there is a path between them, traversing
-- edges in either direction.
components   :: GraphRepresentation node => Graph node -> T.Forest Vertex
components    = dff . undirected
