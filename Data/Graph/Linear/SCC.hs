-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Linear.SCC
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
module Data.Graph.Linear.SCC
  ( SCC(..)
  , stronglyConnComp
  , stronglyConnCompR
  , graphFromEdges'
  , scc
  )
where

-- import Data.Graph.Linear.Basic(Graph, Vertex)
import Debug.Trace
import Data.Graph(Graph, Vertex, SCC(..), graphFromEdges')
import Data.Array.ST
import Data.Array as A
import Data.List(nub, foldl')
import Data.STRef
import Control.Monad(forM_, ap)
import Control.Monad.ST
import Control.Applicative

type SCCList    = [(Int, [Vertex])]
type SCCMap     = Vertex -> Int
type Stack s    = STUArray s Vertex Bool
type Marks s    = STUArray s Vertex Int
type Lowlinks s = STUArray s Vertex Int

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
greaterThanM  = predM (<)

andM      ::  (Applicative m, Monad m) => m Bool -> m Bool -> m Bool
andM       = predM (&&)

predM ::  (Applicative m, Monad m) => (a -> a -> Bool) -> m a -> m a -> m Bool
predM p v1 v2 = p <$> v1 <*> v2

scc :: Graph -> (SCCList, SCCMap)
scc g = runST (
  do marks    <- newArray (bounds g) 0
     lowlinks <- newArray (bounds g) 0
     st       <- newSTRef $ TS [] 1 [] 1
     
     forM_ (indices g) $ \w ->
        whenM (readArray marks w `isEqM` return 0)
            $ strongConnect g marks lowlinks st w

     final <- readSTRef st
     sccMap <- unsafeFreeze marks
     return (sccs final, \i -> sccMap ! i)
  )

{-# INLINE strongConnect #-}
strongConnect :: Graph                -- original graph
               -> Marks s              -- state of node (visited/unvisited)
               -> Lowlinks s
               -- -> Stack s
               -> STRef s TarjanState
               -> Vertex
               -> ST s ()
strongConnect g marks lowlinks st v =
  do s <- readSTRef st
     let n = nextN s
     writeArray marks    v (negate n)
     writeArray lowlinks v n
     let s' =  s { stack = v:stack s
                 , nextN = n + 1
                 }
     writeSTRef st s'

     forM_ (g ! v) $ \w ->
        ifM (readArray marks w `isEqM` return 0)
            (do strongConnect g marks lowlinks st w
                newLowLink <- min <$> readArray lowlinks v <*> readArray lowlinks w
                writeArray lowlinks v newLowLink)
            -- else
            (whenM ((readArray marks w `greaterThanM` readArray marks v) `andM`   -- back edge
                     ((readArray marks w) `lessThanM` return 0))                  -- w on stack
                $ do ll' <- min <$> readArray lowlinks v <*> (negate <$> readArray marks w)
                     writeArray lowlinks v ll')

     whenM (readArray lowlinks v `isEqM` readArray marks v)
         $ do s <- readSTRef st
              let nextComponentID = nextC s
                  (newSCC, newStack) = span (>= v) (stack s)
                  s' =  s { stack = newStack
                          , nextC = nextComponentID + 1
                          , sccs = (nextComponentID, newSCC):sccs s
                          }
              forM_ newSCC $ \i ->
                 writeArray marks i nextComponentID

              writeSTRef st s'

--------------------------------------------------------------------------------
-- | Compute the list of strongly connected components of a graph.
-- The components are topologically sorted:
-- if v1 in C1 points to v2 in C2, then C2 will come before C1 in the list.
sccList :: Graph -> [SCC Vertex]
sccList g = reverse $ map (to_scc g lkp) cs
  where (cs,lkp) = scc g

-- | Compute the list of strongly connected components of a graph.
-- Each component contains the adjecency information from the original graph.
-- The components are topologically sorted:
-- if v1 in C1 points to v2 in C2, then C2 will come before C1 in the list.
sccListR :: Graph -> [SCC (Vertex,[Vertex])]
sccListR g = reverse $ map cvt cs
  where (cs,lkp) = scc g
        cvt (n,[v]) = let adj = g ! v
                      in if  n `elem` map lkp adj
                           then CyclicSCC [(v,adj)]
                           else AcyclicSCC (v,adj)
        cvt (_,vs)  = CyclicSCC [ (v, g ! v) | v <- vs ]

-- | Quotient a graph with the relation that relates vertices that
-- belong to the same SCC.  The vertices in the new graph are the
-- SCCs of the old graph, and there is an edge between two components,
-- if there is an edge between any of their vertices.
-- The entries in the resulting list are in reversed-topologically sorted:
-- if v1 in C1 points to v2 in C2, then C1 will come before C2 in the list.
sccGraph :: Graph -> [(SCC Int, Int, [Int])]
sccGraph g = map to_node cs
  where (cs,lkp) = scc g
        to_node x@(n,this) = ( to_scc g lkp x
                             , n
                             , nub $ concatMap (map lkp . (g !)) this
                             )

stronglyConnComp :: Ord key => [(node, key, [key])] -> [SCC node]
stronglyConnComp es = reverse $ map cvt cs
  where (g,back)    = graphFromEdges' es
        (cs,lkp)    = scc g
        cvt (n,[v]) = let (node,_,_) = back v
                      in if n `elem` map lkp (g ! v)
                            then CyclicSCC [node]
                            else AcyclicSCC node
        cvt (_,vs)  = CyclicSCC [ node | (node,_,_) <- map back vs ]

stronglyConnCompR :: Ord key => [(node, key, [key])] -> [SCC (node, key, [key])]
stronglyConnCompR es = reverse $ map cvt cs
  where (g,back)    = graphFromEdges' es
        (cs,lkp)    = scc g
        cvt (n,[v]) = if n `elem` map lkp (g ! v)
                         then CyclicSCC [back v]
                         else AcyclicSCC (back v)
        cvt (_,vs)  = CyclicSCC (map back vs)

to_scc :: Graph -> (Vertex -> Int) -> (Int,[Vertex]) -> SCC Vertex
to_scc g lkp (n,[v]) = if n `elem` map lkp (g ! v) then CyclicSCC [v]
                                                   else AcyclicSCC v
to_scc _ _ (_,vs)    = CyclicSCC vs

instance Applicative (ST s) where
  pure  = return
  (<*>) = ap
