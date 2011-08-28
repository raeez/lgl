{-# LANGUAGE ExplicitForAll#-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Linear.Representation.Array
-- Copyright   :  
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Data.Graph.Linear.Representation.Array implements the underlying adjacency
-- represenation as a mutable array, as well as a common interface for storing data
-- in this form in the ST monad.
--
-----------------------------------------------------------------------------
--
module Data.Graph.Linear.Representation.Array
(
    Mapping, STMapping
  , mkMap, newSTMap, readSTMap, writeSTMap
  , empty, isEmpty
  , domain, domainBounds
  , (!)
  , unsafeFreeze
)
where
import Data.Array
import Data.Array.ST
-- import Data.Graph.Linear.Representation(LinearMap)

type Mapping a b      = Array a b
type STMapping s a b  = STUArray s a b

mkMap :: Ix i => (i, i) -> [(i, e)] -> Mapping i e
mkMap = array

newSTMap :: (MArray a e m, Ix i) => (i, i) -> e -> m (a i e)
newSTMap = newArray

readSTMap :: (MArray a e m, Ix i) => a i e -> i -> m e
readSTMap = readArray

writeSTMap :: (MArray a e m, Ix i) => a i e -> i -> e -> m ()
writeSTMap = writeArray

empty :: Mapping Int b
empty = array (1,0) []

isEmpty :: Mapping Int b -> Bool
isEmpty m = let (a, b) = bounds m
            in a > b
domain :: Ix i => Mapping i e -> [i]
domain = indices

domainBounds :: Ix i =>  Array i e -> (i, i)
domainBounds = bounds
