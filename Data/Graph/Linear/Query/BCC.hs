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

import Data.Array as A
import Data.Array.ST
import Data.Graph.Linear.Graph

import Data.Graph.Linear.Query.Util
import Data.List(delete, nub, foldl')
import Data.STRef
import qualified Data.Set as S
import Control.Monad(forM_)
import Control.Monad.ST
import Control.Applicative

-- |A list of biconnected components, structured as a tuple of
-- (component id, component edge list)
type BCCList      = [(Int, [Edge Vertex])]

-- |A mapping from internal graph vertices to biconnected component id
type BCCMap       = Vertex -> Int

-- |External interface of a single biconnected component.
-- includes the component identifier, edge list and a function to check node
-- inclusion within this biconnected component
data BCC node = BCC 
  { bccID       :: !Int
  , bccVertices :: [Edge Vertex] -- ^ List of edges in the component
  , bccMap      :: node  -> Maybe Bool
  }

instance Show payload => Show (BCC payload) where
  show (BCC _ vertices _) = show vertices

-- |Structure holding the state of threaded through the execution of Tarjan's
-- strongly connected components algorithm.
data TarjanState = TS
  { nextN :: {-# UNPACK #-} !Int  -- ^ Next node number
  , nextC :: {-# UNPACK #-} !Int  -- ^ next BCC number
  , stack :: ![Edge Vertex]    -- ^ Traversal Stack
  , bccs  :: BCCList    -- ^ bcc list
  }

-- |Run Tarjan's biconnected components algorithm.
bcc :: GraphRepresentation node
    => Graph node
    -> (BCCList, BCCMap)
bcc g = runST (
  do marks      <- newArray (Data.Graph.Linear.Graph.bounds g) 0
     lowpoints  <- newArray (Data.Graph.Linear.Graph.bounds g) 0
     st         <- newSTRef $ TS 1 1 [] []

     forM_ (vertices g) $ \w ->
        whenM (unvisited $ readArray marks w)
            $ biConnect g marks lowpoints st w 0

     final <- readSTRef st
     lkp   <- unsafeFreeze marks

     return (bccs final, \i -> lkp ! i)
  )

{-# INLINE biConnect #-}
biConnect :: GraphRepresentation node
            => Graph node
            -- -> STMapping s Int
            -- -> STMapping s Int
            -> STUArray s Int Int
            -> STUArray s Int Int
            -> STRef s TarjanState
            -> Vertex
            -> Vertex
            -> ST s ()
biConnect g marks lowpoints st v u =
  do s <- readSTRef st
     let n = nextN s
     writeArray marks     v n
     writeArray lowpoints v n
     let s' =  s { nextN = n + 1 }
     writeSTRef st s'

     forM_ (g `adjacentTo` v) $ \w ->
        ifM (unvisited $ readArray marks w)
            (do s <- readSTRef st
                let s' = s { stack = (v, w): stack s }
                writeSTRef st s'

                biConnect g marks lowpoints st w v

                newLowPoint <- min <$> readArray lowpoints v <*> readArray lowpoints w
                writeArray lowpoints v newLowPoint

                whenM (readArray lowpoints w .>=. readArray marks v)
                    $ do s <- readSTRef st
                         let newComponentID = nextC s
                         (newBCC, newStack) <- processStack marks v w (stack s)

                         forM_ newBCC $ \(a, b) ->
                            do writeArray marks a newComponentID
                               writeArray marks b newComponentID

                         let s' = s { nextC = newComponentID + 1
                                    , bccs  = (newComponentID, newBCC):bccs s
                                    , stack = newStack
                                    }
                         writeSTRef st s')
            -- else
            (whenM ((readArray marks w .<. readArray marks v) .&&.
                    (return w ./=. return u))
                $ do s <- readSTRef st
                     let s' = s { stack = (v, w):stack s }
                     writeSTRef st s'
                     lp' <- min <$> readArray lowpoints v <*> readArray marks w
                     writeArray lowpoints v lp')


{-# INLINE processStack #-}
processStack :: STUArray s Vertex Int -> Vertex -> Vertex -> [Edge Vertex] -> ST s ([Edge Vertex], [Edge Vertex])
processStack marks v w stck = do (bcc', stck') <- buildBCC marks w [] stck
                                 return ((v, w):bcc', delete (v, w) stck')

{-# INLINE buildBCC #-}
-- buildBCC :: STMapping s Int -> Vertex -> [Edge Vertex] -> [Edge Vertex] -> ST s ([Edge Vertex], [Edge Vertex])
buildBCC marks w cs [] = return (cs, [])
buildBCC marks w cs ((u1, u2):stck) =
  ifM (readArray marks u1 .>=. readArray marks w)
      (buildBCC marks w ((u1,u2):cs) stck) -- add this edge to the current component
                                           -- and remove from stack
      -- else
      (return (cs, (u1, u2):stck)) -- otherwise return the constructed component

-- | Construct a graph from a list of tuples, and compute the biconnected
-- components∙
biConnectedComp :: Ord label
                  => [(payload, label, [label])]
                  -> [BCC (Node payload label)]

biConnectedComp es = reverse $ map cvt cs
  where g           = mkGraph $ map nodeConstructor es
        (cs, lkp)   = bcc g
        cvt (i, vs) = BCC i vs (\v -> case lkp <$> (grNodeMap g v) of
                                        Just v' -> Just $ v' == i
                                        Nothing -> Nothing)
