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
  )
where

import Data.Graph.Linear.Graph
import Data.Graph.Linear.Representation.Array
import Data.Graph.Linear.Query.Util
import Data.List(nub, foldl')
import Data.STRef
import Control.Monad(forM_, ap)
import Control.Monad.ST
import Control.Applicative

-- | Strongly connected component.
data SCC vertex = AcyclicSCC vertex   -- ^ A single vertex that is not in any cycle.
                | CyclicSCC  [vertex] -- ^ A maximal set of mutually reachable vertices.

instance Functor SCC where
    fmap f (AcyclicSCC v) = AcyclicSCC (f v)
    fmap f (CyclicSCC vs) = CyclicSCC (fmap f vs)

flattenSCCs :: [SCC a] -> [a]
flattenSCCs = concatMap flattenSCC

flattenSCC :: SCC a -> [a]
flattenSCC (AcyclicSCC v) = [v]
flattenSCC (CyclicSCC vs) = vs

type SCCList    = [(Int, [Vertex])]
type SCCMap     = Vertex -> Int
type Marks s    = STMapping s Vertex Int
type Lowlinks s = STMapping s Vertex Int

data TarjanState = TS
  { nextN :: {-# UNPACK #-} !Int      -- ^ Next node number
  , nextC :: {-# UNPACK #-} !Int      -- ^ next SCC number
  , stack :: ![Vertex]                -- ^ Traversal Stack
  , sccs  :: !SCCList  -- ^ Completed scc list
  }

scc :: GraphRepresentation node => Graph node -> (SCCList, SCCMap)
scc g = runST (
  do marks    <- newSTMap (bounds g) 0
     lowlinks <- newSTMap (bounds g) 0
     st       <- newSTRef $ TS 1 1 [] []
     
     forM_ (vertices g) $ \w ->
        whenM (unvisited marks w) -- unvisited
            $ strongConnect g marks lowlinks st w

     final <- readSTRef st
     sccMap <- unsafeFreeze marks
     return (sccs final, \i -> sccMap ! i)
  )

{-# INLINE strongConnect #-}
strongConnect :: GraphRepresentation node
              => Graph node                -- original graph
               -> STMapping s Vertex Int   -- state of node (visited/unvisited)
               -> STMapping s Vertex Int
               -> STRef s TarjanState
               -> Vertex
               -> ST s ()
strongConnect g marks lowlinks st v =
  do s <- readSTRef st
     let n = nextN s
     writeSTMap marks    v (negate n)
     writeSTMap lowlinks v n
     let s' =  s { stack = v:stack s
                 , nextN = n + 1
                 }
     writeSTRef st s'

     forM_ (g `adjacentTo` v) $ \w ->
        ifM (unvisited marks w)
            (do strongConnect g marks lowlinks st w
                newLowLink <- min <$> readSTMap lowlinks v <*> readSTMap lowlinks w
                writeSTMap lowlinks v newLowLink)
            -- else
            (whenM ((readSTMap marks w .>. readSTMap marks v) .&&.   -- back edge
                     ((readSTMap marks w) .<. return 0))                  -- w on stack
                $ do ll' <- min <$> readSTMap lowlinks v <*> (negate <$> readSTMap marks w)
                     writeSTMap lowlinks v ll')

     whenM (readSTMap lowlinks v .==. readSTMap marks v)
         $ do s <- readSTRef st
              let nextComponentID = nextC s
                  (newSCC, newStack) = span (>= v) (stack s)
                  s' =  s { stack = newStack
                          , nextC = nextComponentID + 1
                          , sccs = (nextComponentID, newSCC):sccs s
                          }
              forM_ newSCC $ \i ->
                 writeSTMap marks i nextComponentID

              writeSTRef st s'


stronglyConnComp :: Ord label
                  => [(payload, label, [label])]
                  -> [SCC payload]
stronglyConnComp es = reverse $ map cvt cs
  where g           = mkGraph $ map (\(p, l, ls) -> Node p l ls) es
        (cs,lkp)    = scc g
        cvt (n,[v]) = let Node payload _ _ = grVertexMap g v
                      in if n `elem` map lkp (g `adjacentTo` v)
                          then CyclicSCC [payload]
                          else AcyclicSCC payload
        cvt (_,vs)  = CyclicSCC [ payload | Node payload _ _ <- map (grVertexMap g) vs ]


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

-----------------------------------------------------------------------------
{-
type WorkItem key payload
  = (Node key payload,	-- Tip of the path
     [payload])	  	-- Rest of the path; 
     			--  [a,b,c] means c depends on b, b depends on a

-- | Find a reasonably short cycle a->b->c->a, in a strongly
-- connected component.  The input nodes are presumed to be
-- a SCC, so you can start anywhere.
findCycle :: forall payload key. Ord key 
          => [Node key payload]     -- The nodes.  The dependencies can
	     	     	  	    -- contain extra keys, which are ignored
	  -> Maybe [payload]        -- A cycle, starting with node
	     			    -- so each depends on the next
findCycle graph
  = go Set.empty (new_work root_deps []) []
  where
    env :: Map.Map key (Node key payload)
    env = Map.fromList [ (key, node) | node@(_, key, _) <- graph ]

    -- Find the node with fewest dependencies among the SCC modules
    -- This is just a heuristic to find some plausible root module
    root :: Node key payload
    root = fst (minWith snd [ (node, count (`Map.member` env) deps) 
                            | node@(_,_,deps) <- graph ])
    (root_payload,root_key,root_deps) = root


    -- 'go' implements Dijkstra's algorithm, more or less
    go :: Set.Set key	-- Visited
       -> [WorkItem key payload]	-- Work list, items length n
       -> [WorkItem key payload]	-- Work list, items length n+1
       -> Maybe [payload]		-- Returned cycle
       -- Invariant: in a call (go visited ps qs),
       --            visited = union (map tail (ps ++ qs))

    go _       [] [] = Nothing	-- No cycles
    go visited [] qs = go visited qs []
    go visited (((payload,key,deps), path) : ps) qs 
       | key == root_key           = Just (root_payload : reverse path)
       | key `Set.member` visited  = go visited ps qs
       | key `Map.notMember` env   = go visited ps qs
       | otherwise                 = go (Set.insert key visited)
                                        ps (new_qs ++ qs)
       where
	 new_qs = new_work deps (payload : path)

    new_work :: [key] -> [payload] -> [WorkItem key payload]
    new_work deps path = [ (n, path) | Just n <- map (`Map.lookup` env) deps ]
-}

{-
--------------------------------------------------------------------------------
-- | Compute the list of strongly connected components of a graph.
-- The components are topologically sorted:
-- if v1 in C1 points to v2 in C2, then C2 will come before C1 in the list.
sccList :: GraphRepresentation node => Graph node -> [SCC Vertex]
sccList g = reverse $ map (to_scc g lkp) cs
  where (cs,lkp) = scc g

-- | Compute the list of strongly connected components of a graph.
-- Each component contains the adjecency information from the original graph.
-- The components are topologically sorted:
-- if v1 in C1 points to v2 in C2, then C2 will come before C1 in the list.
sccListR :: GraphRepresentation node => Graph node -> [SCC (Vertex,[Vertex])]
sccListR g = reverse $ map cvt cs
  where (cs,lkp) = scc g
        cvt (n,[v]) = let adj = g `adjacentTo` v
                      in if  n `elem` map lkp adj
                           then CyclicSCC [(v,adj)]
                           else AcyclicSCC (v,adj)
        cvt (_,vs)  = CyclicSCC [ (v, g `adjacentTo` v) | v <- vs ]

-- | Quotient a graph with the relation that relates vertices that
-- belong to the same SCC.  The vertices in the new graph are the
-- SCCs of the old graph, and there is an edge between two components,
-- if there is an edge between any of their vertices.
-- The entries in the resulting list are in reversed-topologically sorted:
-- if v1 in C1 points to v2 in C2, then C1 will come before C2 in the list.
sccGraph :: GraphRepresentation node
          => Graph node
          -> [(SCC Int, Int, [Int])]
sccGraph g = map to_node cs
  where (cs,lkp) = scc g
        to_node x@(n,this) = ( to_scc g lkp x
                             , n
                             , nub $ concatMap (map lkp . (g `adjacentTo`)) this
                             )

to_scc :: GraphRepresentation node
        => Graph node
        -> (Vertex -> Int)
        -> (Int,[Vertex])
        -> SCC Vertex
to_scc g lkp (n,[v]) = if n `elem` map lkp (g `adjacentTo` v) then CyclicSCC [v]
                                                              else AcyclicSCC v
to_scc _ _ (_,vs)    = CyclicSCC vs
-}
