-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Linear.Representation.Vector
-- Copyright   :  
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Data.Graph.Linear.Representation.Vector implements the underlying graph adjacency
-- information as a mutable unboxed vector, as well as a common interface for storing data
-- in this form in the ST monad.
--
-----------------------------------------------------------------------------
--
module Data.Graph.Linear.Representation.Vector
(
    Bounds
  , Mapping, STMapping
  , fromList, newSTMap, readSTMap, writeSTMap
  , empty, isEmpty
  , domain, domainBounds
  , (!)
  , unsafeFreeze
)
where
import Control.Monad.Primitive
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Mutable as MV
import Data.List

-- |Represents the upper and lower bounds of the domain of the Mapping.

type Bounds = (Int, Int)
-- |Represents an array mapping integer indices to values of type 'a'. Uses
-- unboxed vectors.
type Mapping e = V.Vector e

-- |Given a state type 's', STMapping s a represents an array mapping integer
-- indices to values of type 'a' within the ST monad. Uses unboxed vectors.
type STMapping s e = MV.STVector s e

-- |Construct a Mapping from a given list of elements.
fromList :: [(Int, e)] -> Mapping e
fromList vs = V.update new  (V.fromList vs)
  where
    new = V.replicate (length vs) undefined

-- |The empty Mapping.
empty :: Mapping e
empty = V.empty

-- |Test for the empty Mapping.
isEmpty :: Mapping e -> Bool
isEmpty = V.null

-- |List representing the domain of the Mapping.
domain :: Mapping e -> [Int]
domain m = [0..V.length m - 1]

-- |The upper and lower bounds of the domain of the Mapping.
domainBounds :: Mapping a -> (Int, Int)
domainBounds m = (0, V.length m - 1)

-- |O(1) element indexing without bounds checking.
(!) :: Mapping a -> Int -> a
(!) m i = m `V.unsafeIndex` i

-- |Builds a new Mapping, with every element initialised to the supplied value.
newSTMap :: PrimMonad m => (t, Int) -> a -> m (MV.MVector (PrimState m) a)
newSTMap (low, hi) e = MV.new hi >>= writeDefaults hi
  where
    writeDefaults 0 v = return v
    writeDefaults n v = writeSTMap v n e >> writeDefaults (n-1) v

-- |Read an element from an existing STMapping.
readSTMap :: PrimMonad m => MV.MVector (PrimState m) a -> Int -> m a
readSTMap = MV.unsafeRead

-- |Write an element to an existing STMapping.
writeSTMap :: PrimMonad m => MV.MVector (PrimState m) a -> Int -> a -> m ()
writeSTMap = MV.unsafeWrite

-- |O(1) Unsafe convert a Mapping to an immutable one without copying.
-- The mapping may not be used after this operation.
unsafeFreeze :: (UV.Unbox a, PrimMonad m) => UV.MVector (PrimState m) a -> m (UV.Vector a)
unsafeFreeze = UV.unsafeFreeze
