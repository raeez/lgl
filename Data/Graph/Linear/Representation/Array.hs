{-# LANGAGE NoMonomorphismRestriction #-}
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
-- Data.Graph.Linear.Representation.Array implements the underlying graph adjacency
-- information as a mutable array, as well as a common interface for storing data
-- in this form in the ST monad.
--
-----------------------------------------------------------------------------
--
module Data.Graph.Linear.Representation.Array
(
    Bounds
  , Mapping, STMapping
  , mkMap, newSTMap, readSTMap, writeSTMap
  , (!)
  , empty, isEmpty
  , domain, domainBounds
  , unsafeFreeze
)
where
-- import qualified Data.Array.Unboxed as A
import qualified Data.Array as A
import qualified Data.Array.IArray as IA
import qualified Data.Array.ST as MA

-- |Represents the upper and lower bounds of the domain of a given Mapping.
type Bounds = (Int, Int)

-- |Represents a structure mapping integer indices to values of type 'a'. Uses
-- boxed arrays.
type Mapping e =  A.Array Int e

-- |Given a state type 's', STMapping s a represents an array mapping integer
-- indices to values of type 'a' within the ST monad. Uses unboxed arrays.
type STMapping s e = MA.STArray s Int e

-- |Construct a Mapping from a given list of elements
mkMap :: Bounds -> [(Int, e)] -> Mapping e
mkMap bnds items = A.array bnds items

-- |The empty Mapping
empty :: Mapping e
empty = A.array (1,0) []

-- |Test for the empty Mapping.
isEmpty :: Mapping e -> Bool
isEmpty m = let (a, b) = A.bounds m in a > b

-- |List representing the domain of the Mapping.
domain :: Mapping e -> [Int] 
domain = A.indices

-- |The upper and lower bounds of the domain of the Mapping.
domainBounds :: Mapping e -> Bounds
domainBounds = A.bounds

-- |O(1) element indexing
(!) :: Mapping a -> Int -> a
(!) = (A.!)
{-# INLINE (!) #-}

-- |Builds a new STMapping, with every element initialised to the supplied value.
newSTMap :: MA.MArray a e m =>  Bounds-> e -> m (a Int e)
newSTMap = MA.newArray

-- |Read an element from an existing STMapping.
readSTMap :: MA.MArray a e m => a Int e -> Int -> m e
readSTMap = MA.readArray
{-# INLINE readSTMap #-}

-- |Write an element to an existing STMapping.
writeSTMap :: MA.MArray a e m => a Int e -> Int -> e -> m ()
writeSTMap = MA.writeArray
{-# INLINE writeSTMap #-}

-- |O(1) Unsafe convert a Mapping to an immutable one without copying.
-- The mapping may not be used after this operation.
unsafeFreeze :: (MA.MArray a e m, IA.IArray b e) => a Int e -> m (b Int e)
unsafeFreeze = MA.unsafeFreeze
