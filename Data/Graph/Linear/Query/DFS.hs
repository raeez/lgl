{-# LANGUAGE RankNTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Linear.Query.DFS
-- Copyright   :  
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Data.Graph.Linear.DFS implements a depth first search traversal of the given
-- graph.
--
-----------------------------------------------------------------------------
--

module Data.Graph.Linear.Query.DFS
(
    dff
  , dfs
)
where

import Control.Applicative
import Control.Monad(ap)
import Control.Monad.ST
import Data.Array.ST
import Data.Graph.Linear.Graph
import Data.Tree as T

-- | A spanning forest of the graph, obtained from a depth-first search of
-- the graph starting from each vertex in an unspecified order.
dff :: GraphRepresentation node => Graph node -> T.Forest Vertex
dff g = dfs g (vertices g)

-- | A spanning forest of the part of the graph reachable from the listed
-- vertices, obtained from a depth-first search of the graph starting at
-- each of the listed vertices in order.
dfs          :: GraphRepresentation node => Graph node -> [Vertex] -> T.Forest Vertex
dfs g vs      = prune (bounds g) (map (generate g) vs)

generate     :: GraphRepresentation node => Graph node -> Vertex -> Tree Vertex
generate g v  = T.Node v (map (generate g) (g `adjacentTo` v))

prune        :: Bounds -> T.Forest Vertex -> T.Forest Vertex
prune bnds ts = run bnds (chop ts)

chop         :: T.Forest Vertex -> SetM s (T.Forest Vertex)
chop []       = return []
chop (T.Node v ts : us)
              = do visited <- contains v
                   if visited
                   then chop us
                   else do include v
                           as <- chop ts
                           bs <- chop us
                           return (T.Node v as : bs)

-- A monad holding a set of vertices visited so far.

newtype SetM s a = SetM { runSetM :: STUArray s Vertex Bool -> ST s a }

instance Functor (SetM s) where
    fmap f m = m >>= return . f

instance Applicative (SetM s) where
    pure = return
    (<*>) = ap

instance Monad (SetM s) where
    return x     = SetM $ const (return x)
    SetM v >>= f = SetM $ \ s -> do { x <- v s; runSetM (f x) s }

run          :: Bounds -> (forall s. SetM s a) -> a
run bnds act  = runST (newArray bnds False >>= runSetM act)

contains     :: Vertex -> SetM s Bool
contains v    = SetM $ \ m -> readArray m v

include      :: Vertex -> SetM s ()
include v     = SetM $ \ m -> writeArray m v True
