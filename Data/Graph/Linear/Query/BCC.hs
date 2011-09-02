-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Linear.Query.BCC
-- Copyright   :  
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Data.Graph.Linear.BCC implements tarjan's biconnected components
-- algorithm, along with a series of BCC manipulation functions.
--
-----------------------------------------------------------------------------
--
module Data.Graph.Linear.Query.BCC
( 
    BCC(..)
  , biConnectedComp
  , bcc
)
where

import Data.Graph.Linear.Graph
import Data.Graph.Linear.Basic
import Data.Graph.Linear.Query.DFS
import Data.Graph.Linear.Query.Util
import qualified Data.Tree as T

import Data.List(delete, nub, foldl', find)

-- |A list of biconnected components, structured as a tuple of
-- (component id, component edge list)
type BCCList      = [(Int, [Edge Vertex])]

-- |A mapping from internal graph vertices to biconnected component id
type BCCMap       = Vertex -> Int

-- |External interface of a single biconnected component.
-- includes the component identifier, edge list and a function to check node
-- inclusion within this biconnected component
data BCC node = BCC 
  { bccVertices :: [Vertex] -- ^ List of edges in the component
  }

instance Show payload => Show (BCC payload) where
  show (BCC vertices) = show vertices

instance Eq a => Eq (BCC a) where
  (BCC vs1) == (BCC vs2) = all (`elem` vs2) vs1 && all (`elem` vs1) vs2

-- |Run Tarjan's biconnected components algorithm.
bcc :: GraphRepresentation node
      => Graph node
      -> T.Forest [Vertex]
bcc g = (concat . map bicomps . map (do_label g dnum)) forest
 where forest = dff g
       dnum   = preArr (bounds g) forest

do_label :: GraphRepresentation node
          => Graph node
          -> (Int -> Int)
          -> T.Tree Vertex
          -> T.Tree (Vertex,Int,Int)
do_label g dnum (T.Node v ts) = T.Node (v,dnum v,lv) us
 where us = map (do_label g dnum) ts
       lv = minimum ([dnum v] ++ [dnum w | w <- g `adjacentTo` v]
                     ++ [lu | T.Node (_,_,lu) _ <- us])

bicomps :: T.Tree (Vertex,Int,Int) -> T.Forest [Vertex]
bicomps (T.Node (v,_,_) ts)
      = [ T.Node (v:vs) us | (_,T.Node vs us) <- map collect ts]

collect :: T.Tree (Vertex,Int,Int) -> (Int, T.Tree [Vertex])
collect (T.Node (v,dv,lv) ts) = (lv, T.Node (v:vs) cs)
 where collected = map collect ts
       vs = concat [ ws | (lw, T.Node ws _) <- collected, lw<dv]
       cs = concat [ if lw<dv then us else [T.Node (v:ws) us]
                        | (lw, T.Node ws us) <- collected ]

-- | Construct a graph from a list of tuples, and compute the biconnected
-- components∙
biConnectedComp :: Ord label
                  => [(payload, label, [label])]
                  -> [BCC (Node payload label)]

biConnectedComp es = reverse $ map BCC cs
  where g           = mkGraph $ map nodeConstructor es
        cs          = concatMap T.flatten $ bcc g
