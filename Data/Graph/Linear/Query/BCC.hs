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
import Data.Graph.Linear.Representation.Array
import Data.Graph.Linear.Query.Util
import Data.List(delete, nub, foldl')
import Data.STRef
import qualified Data.Set as S
import Control.Monad(forM_)
import Control.Monad.ST
import Control.Applicative

type BCCList      = [(Int, [Edge])]
type BCCMap       = Vertex -> Int

data BCC node = BCC 
  { bccID       :: !Int
  , bccVertices :: [Edge] -- ^ List of edges in the component
  , bccMap      :: node  -> Maybe Bool
  }

instance Show payload => Show (BCC payload) where
  show (BCC _ vertices _) = show vertices


data TarjanState = TS
  { nextN :: {-# UNPACK #-} !Int  -- ^ Next node number
  , nextC :: {-# UNPACK #-} !Int  -- ^ next BCC number
  , stack :: ![Edge]    -- ^ Traversal Stack
  , bccs  :: BCCList    -- ^ bcc list
  }

bcc :: GraphRepresentation node
    => Graph node
    -> (BCCList, BCCMap)
bcc g = runST (
  do marks      <- newSTMap (bounds g) 0
     lowpoints  <- newSTMap (bounds g) 0
     st         <- newSTRef $ TS 1 1 [] []

     forM_ (vertices g) $ \w ->
        whenM (unvisited marks w)
            $ biConnect g marks lowpoints st w 0

     final <- readSTRef st
     lkp   <- unsafeFreeze marks

     return (bccs final, \i -> lkp ! i)
  )

{-# INLINE biConnect #-}
biConnect :: GraphRepresentation node
            => Graph node
            -> STMapping s Vertex Int
            -> STMapping s Vertex Int
            -> STRef s TarjanState
            -> Vertex
            -> Vertex
            -> ST s ()
biConnect g marks lowpoints st v u =
  do s <- readSTRef st
     let n = nextN s
     writeSTMap marks     v n
     writeSTMap lowpoints v n
     let s' =  s { nextN = n + 1 }
     writeSTRef st s'

     forM_ (g `adjacentTo` v) $ \w ->
        ifM (unvisited marks w)
            (do s <- readSTRef st
                let s' = s { stack = (v, w): stack s }
                writeSTRef st s'

                biConnect g marks lowpoints st w v

                newLowPoint <- min <$> readSTMap lowpoints v <*> readSTMap lowpoints w
                writeSTMap lowpoints v newLowPoint

                whenM (readSTMap lowpoints w .>=. readSTMap marks v)
                    $ do s <- readSTRef st
                         let newComponentID = nextC s
                         (newBCC, newStack) <- processStack marks v w (stack s)

                         forM_ newBCC $ \(a, b) ->
                            do writeSTMap marks a newComponentID
                               writeSTMap marks b newComponentID

                         let s' = s { nextC = newComponentID + 1
                                    , bccs  = (newComponentID, newBCC):bccs s
                                    , stack = newStack
                                    }
                         writeSTRef st s')
            -- else
            (whenM ((readSTMap marks w .<. readSTMap marks v) .&&.
                    (return w ./=. return u))
                $ do s <- readSTRef st
                     let s' = s { stack = (v, w):stack s }
                     writeSTRef st s'
                     lp' <- min <$> readSTMap lowpoints v <*> readSTMap marks w
                     writeSTMap lowpoints v lp')


{-# INLINE processStack #-}
processStack :: STMapping s Vertex Int -> Vertex -> Vertex -> [Edge] -> ST s ([Edge], [Edge])
processStack marks v w stck = do (bcc', stck') <- buildBCC marks w [] stck
                                 return ((v, w):bcc', delete (v, w) stck')

buildBCC :: STMapping s Vertex Int -> Vertex -> [Edge] -> [Edge] -> ST s ([Edge], [Edge])
buildBCC marks w cs [] = return (cs, [])
buildBCC marks w cs ((u1, u2):stck) =
  ifM (readSTMap marks u1 .>=. readSTMap marks w)
      (buildBCC marks w ((u1,u2):cs) stck) -- add this edge to the current component
                                           -- and remove from stack
      -- else
      (return (cs, (u1, u2):stck)) -- otherwise return the constructed component

biConnectedComp :: Ord label
                  => [(payload, label, [label])]
                  -> [BCC (Node payload label)]

biConnectedComp es = reverse $ map cvt cs
  where g           = mkGraph $ map nodeConstructor es
        (cs, lkp)   = bcc g
        cvt (i, vs) = BCC i vs (\v -> case lkp <$> (grNodeMap g v) of
                                        Just v' -> Just $ v' == i
                                        Nothing -> Nothing)
