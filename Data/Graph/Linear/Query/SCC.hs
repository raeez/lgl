{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Linear.Query.SCC
-- Copyright   :  (c) The University of Glasgow 2002
--                (c) The University of Glasgow 2006
--                (c) Raeez Lorgat 2011
-- License     :  BSD-style (see the file libraries/base/LICENSE)
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
  , stronglyConnComp
  -- , stronglyConnCompR
  , scc
  )
where

import Debug.Trace
import Data.Graph.Linear.Graph
import Data.Graph.Linear.Representation.Array
import Data.List(nub, foldl')
import Data.STRef
import Control.Monad(forM_, ap)
import Control.Monad.ST
import Control.Applicative

-------------------------------------------------------------------------
--									-
--	External interface
--									-
-------------------------------------------------------------------------
--
-- | Strongly connected component.
data SCC vertex = AcyclicSCC vertex	-- ^ A single vertex that is not
					-- in any cycle.
	        | CyclicSCC  [vertex]	-- ^ A maximal set of mutually
					-- reachable vertices.

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
  {  stack :: {-# UNPACK #-} ![Vertex] -- ^ Traversal stack
  ,  nextN :: {-# UNPACK #-} !Int      -- ^ Next node number
  ,  sccs  :: {-# UNPACK #-} !SCCList  -- ^ Completed scc list
  ,  nextC :: {-# UNPACK #-} !Int      -- ^ Next SCC number
  }

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p tb fb = p >>= \pred -> if pred then tb else fb

whenM :: Monad m => m Bool -> m a -> m ()
whenM p tb = p >>= \pred -> if pred then tb >> return () else return ()

isEqM      :: (Applicative m, Monad m, Eq a) => m a -> m a -> m Bool
isEqM      = predM (==)

lessThanM  :: (Applicative m, Monad m, Ord a) => m a -> m a -> m Bool
lessThanM  = predM (<)

greaterThanM  :: (Applicative m, Monad m, Ord a) => m a -> m a -> m Bool
greaterThanM  = predM (>)

andM      ::  (Applicative m, Monad m) => m Bool -> m Bool -> m Bool
andM       = predM (&&)

predM ::  (Applicative m, Monad m) => (a -> a -> Bool) -> m a -> m a -> m Bool
predM p v1 v2 = p <$> v1 <*> v2

scc :: GraphRepresentation node => Graph node -> (SCCList, SCCMap)
scc g = runST (
  do marks    <- newSTMap (bounds g) 0
     lowlinks <- newSTMap (bounds g) 0
     st       <- newSTRef $ TS [] 1 [] 1
     
     forM_ (vertices g) $ \w ->
        whenM (readSTMap marks w `isEqM` return 0) -- unvisited
            $ strongConnect g marks lowlinks st w

     final <- readSTRef st
     sccMap <- unsafeFreeze marks
     return (sccs final, \i -> sccMap ! i)
  )

{-# INLINE strongConnect #-}
strongConnect :: GraphRepresentation node
              => Graph node                -- original graph
               -> Marks s              -- state of node (visited/unvisited)
               -> Lowlinks s
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
        ifM (readSTMap marks w `isEqM` return 0)
            (do strongConnect g marks lowlinks st w
                newLowLink <- min <$> readSTMap lowlinks v <*> readSTMap lowlinks w
                writeSTMap lowlinks v newLowLink)
            -- else
            (whenM ((readSTMap marks w `greaterThanM` readSTMap marks v) `andM`   -- back edge
                     ((readSTMap marks w) `lessThanM` return 0))                  -- w on stack
                $ do ll' <- min <$> readSTMap lowlinks v <*> (negate <$> readSTMap marks w)
                     writeSTMap lowlinks v ll')

     whenM (readSTMap lowlinks v `isEqM` readSTMap marks v)
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
sccGraph :: GraphRepresentation node => Graph node -> [(SCC Int, Int, [Int])]
sccGraph g = map to_node cs
  where (cs,lkp) = scc g
        to_node x@(n,this) = ( to_scc g lkp x
                             , n
                             , nub $ concatMap (map lkp . (g `adjacentTo`)) this
                             )

stronglyConnComp :: (GraphRepresentation (Node payload label),  Ord label) => [(payload, label, [label])] -> [SCC payload]
stronglyConnComp es = reverse $ map cvt cs
  where (g,back)    = (\x -> (mkGraph x , mkBack x)) $ map (\(p, l, ls) -> Node p l ls) es
        (cs,lkp)    = scc g
        cvt (n,[v]) = let Node payload _ _ = back v
                      in if n `elem` map lkp (g `adjacentTo` v)
                            then CyclicSCC [payload]
                            else AcyclicSCC payload
        cvt (_,vs)  = CyclicSCC [ payload | Node payload _ _ <- map back vs ]

{-
stronglyConnCompR :: (GraphRepresentation (Node payload label), Ord label) => [(payload, label, [label])] -> [SCC (payload, label, [label])]
stronglyConnCompR es = reverse $ map cvt cs
  where (g,back)    = mkGraph $ map (\(p, l, ls) -> Node p l ls) es
        (cs,lkp)    = scc g
        cvt (n,[v]) = if n `elem` map lkp (g ! v)
                         then CyclicSCC [back v]
                         else AcyclicSCC (back v)
        cvt (_,vs)  = CyclicSCC (map back vs)
        -}

to_scc :: GraphRepresentation node => Graph node -> (Vertex -> Int) -> (Int,[Vertex]) -> SCC Vertex
to_scc g lkp (n,[v]) = if n `elem` map lkp (g `adjacentTo` v) then CyclicSCC [v]
                                                              else AcyclicSCC v
to_scc _ _ (_,vs)    = CyclicSCC vs

instance Applicative (ST s) where
  pure  = return
  (<*>) = ap




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
