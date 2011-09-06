-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Linear.Query.SCC
-- Copyright   :  
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Data.Graph.Linear.SCC implements tarjan's strongly connected components
-- algorithm, along with a series of SCC manipulation functions.
--
-----------------------------------------------------------------------------
--
module Data.Graph.Linear.Query.SCC
  ( SCC(..)
  , stronglyConnComp, stronglyConnCompN
  , scc
  , flattenSCCs, flattenSCC
  )
where

import Data.Graph.Linear.Graph as G
import Data.Array as A
import Data.Array.ST

import Data.Graph.Linear.Query.Util
import Data.Graph.Linear.Query.DFS
import Data.List(nub, foldl')
import Data.STRef
import Control.Monad(forM_, ap)
import Control.Monad.ST
import Control.Applicative

-- |Strongly connected component.
data SCC vertex = AcyclicSCC vertex   -- ^ A single vertex that is not in any cycle.
                | CyclicSCC  [vertex] -- ^ A maximal set of mutually reachable vertices.

instance Functor SCC where
    fmap f (AcyclicSCC v) = AcyclicSCC (f v)
    fmap f (CyclicSCC vs) = CyclicSCC (fmap f vs)

instance Eq a => Eq(SCC a) where
    (CyclicSCC _)   == (AcyclicSCC _)  = False
    (AcyclicSCC _)  == (CyclicSCC _)   = False
    (CyclicSCC v1)  == (CyclicSCC v2)  = all (`elem` v2) v1 && all (`elem` v1) v2
    (AcyclicSCC v1) == (AcyclicSCC v2) = v1 == v2

instance Show s => Show (SCC s) where
    show (AcyclicSCC v) = show v
    show (CyclicSCC vs) = show vs

-- |Convert a list of SCCs to its list representation.
flattenSCCs :: [SCC a] -> [a]
flattenSCCs = concatMap flattenSCC

-- |Convert a single SCC to its list representation.
flattenSCC :: SCC a -> [a]
flattenSCC (AcyclicSCC v) = [v]
flattenSCC (CyclicSCC vs) = vs

-- |Output of Tarjan's strongly connected components algorithm; returns a list
-- of tuples, indexed by (component id, list of vertices in component).
type SCCList    = [(Int, [Vertex])]

-- |Output of Tarjan's strongly connected components algorithm; returns a map from
-- vertices to strongly connected component id's.
type SCCMap     = Vertex -> Int

-- |Structure holding the state threaded through the execution of Tarjan's
-- strongly connected components algorithm.
data TarjanState = TS
  { nextN :: {-# UNPACK #-} !Int  -- ^ Next node number
  , nextC :: {-# UNPACK #-} !Int  -- ^ next SCC number
  , stack :: ![Vertex]            -- ^ Traversal Stack
  , sccs  :: !SCCList             -- ^ Completed scc list
  }

-- |Run Tarjan's strongly connected components algorithm.
scc :: GraphRepresentation node => Graph node -> (SCCList, SCCMap)
scc g = runST (
  do marks    <- newArray (G.bounds g) 0
     lowlinks <- newArray (G.bounds g) 0
     st       <- newSTRef $ TS 1 1 [] []

     forM_ (vertices g) $ \w ->
        whenM (unvisited $ readArray marks w)
            $ strongConnect g marks lowlinks st w

     final <- readSTRef st
     sccMap <- unsafeFreeze marks

     return (sccs final, (!) sccMap)
  )

{-# INLINE strongConnect #-}
-- |Find the strongly connected components rooted at vertex v
strongConnect :: GraphRepresentation node
              => Graph node                -- original graph
               -> STUArray s Vertex Int
               -> STUArray s Vertex Int
               -> STRef s TarjanState
               -> Vertex
               -> ST s ()
strongConnect g marks lowlinks st v =
  do s1 <- readSTRef st
     let n = nextN s1
     writeArray marks    v (negate n)
     writeArray lowlinks v n
     let s1' =  s1 { stack = v:stack s1
                   , nextN = n + 1
                   }
     writeSTRef st s1'

     forM_ (g `adjacentTo` v) $ \w ->
        do wm <- readArray marks w
           ifM (unvisited $ return wm)
               (do strongConnect g marks lowlinks st w
                   newLowLink <- min <$> readArray lowlinks v <*> readArray lowlinks w
                   writeArray lowlinks v newLowLink)
               -- else
               (do (whenM (return wm .<. return 0) -- is a back edge + is on stack
                     $ do ll' <- min <$> readArray lowlinks v <*> (negate <$> return wm)
                          writeArray lowlinks v ll'))

     whenM (readArray lowlinks v .>=. return n)
         $ do s2 <- readSTRef st
              let nextComponentID = nextC s2
                  (newSCC', h:newStack) = span (/= v) (stack s2)
                  newSCC = h:newSCC'
                  s2' =  s2 { stack = newStack
                            , nextC = nextComponentID + 1
                            , sccs = (nextComponentID, newSCC):sccs s2
                            }

              forM_ newSCC $ \i ->
                 do writeArray marks i nextComponentID

              writeSTRef st s2'

-- |Construct a graph from a list of tuples and compute the strongly connected
-- components.
stronglyConnComp :: Ord label
                  => [(payload, label, [label])]
                  -> [SCC payload]
stronglyConnComp es = reverse $ map cvt cs
  where g           = mkGraph $ map nodeConstructor es
        (cs,lkp)    = scc g
        cvt (n,[v]) = let Node payload _ _ = grVertexMap g v
                      in if n `elem` map lkp (g `adjacentTo` v)
                          then CyclicSCC [payload]
                          else AcyclicSCC payload
        cvt (_,vs)  = CyclicSCC [ payload | Node payload _ _ <- map (grVertexMap g) vs ]


-- |Wrapper function computing the strongly connected components present in the
-- graph represented as a list of tuples. Same as stronglyConnComp, but retains
-- full node information in the generated SCCs.
stronglyConnCompN :: Ord label
                  => [(payload, label, [label])]
                  -> [SCC (Node payload label)]
stronglyConnCompN es = reverse $ map cvt cs
  where g            = mkGraph $ map nodeConstructor es
        (cs,lkp)     = scc g
        cvt (n,[v])  = if n `elem` map lkp (g `adjacentTo` v)
                         then CyclicSCC [grVertexMap g v]
                         else AcyclicSCC (grVertexMap g v)
        cvt (_,vs)   = CyclicSCC (map (grVertexMap g) vs)
