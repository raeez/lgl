module Data.Graph.Linear.Query.Util where

import Control.Applicative
import Control.Monad(ap)
import Control.Monad.ST

import Data.Array as A
import Data.Array.ST

import Data.Graph.Linear.Graph
-- import Data.Graph.Linear.Representation.Array

{-# INLINE unvisited #-}
-- | Monadic action returning true if a node is unvisited in a graph traversal.
-- unvisited :: STMapping s Int -> Vertex -> ST s Bool
unvisited :: ST s Int ->  ST s Bool
-- unvisited marks w = readSTMap marks w .==. return 0
unvisited wm = wm .==. return 0

{-# INLINE ifM #-}
-- | (restricted) Monadic if. Both branches must return the same value.
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p tb fb = p >>= \pred -> case pred of
                              True -> tb
                              False -> fb

{-# INLINE whenM #-}
-- | Monadic when. Equivalent to single-arm if.
whenM :: Monad m => m Bool -> m a -> m ()
whenM p tb = p >>= \pred -> case pred of
                            True -> tb >> return ()
                            False -> return ()

{-# INLINE (.==.) #-}
-- |Applicative equality operator for lifted values.
(.==.) :: (Applicative m, Monad m, Eq a) => m a -> m a -> m Bool
(.==.) = predM (==)

{-# INLINE (./=.) #-}
-- |Applicative inequality operator for lifted values.
(./=.) :: (Applicative m, Monad m, Eq a) => m a -> m a -> m Bool
(./=.) = predM (/=)

{-# INLINE (.<.) #-}
-- |Applicative less-than operator for lifted values.
(.<.) :: (Applicative m, Monad m, Ord a) => m a -> m a -> m Bool
(.<.) = predM (<)

{-# INLINE (.>.) #-}
-- |Applicative greater-than operator for lifted values.
(.>.) :: (Applicative m, Monad m, Ord a) => m a -> m a -> m Bool
(.>.) = predM (>)

{-# INLINE (.>=.) #-}
-- |Applicative greater-than-equals operator for lifted values.
(.>=.) :: (Applicative m, Monad m, Ord a) => m a -> m a -> m Bool
(.>=.) = predM (>=)

{-# INLINE (.&&.) #-}
-- |Applicative logical and operator for lifted values.
(.&&.) ::  (Applicative m, Monad m) => m Bool -> m Bool -> m Bool
(.&&.) = predM (&&)

{-# INLINE predM #-}
-- |Helper function for constructing monadic predicate operators.
predM ::  (Applicative m, Monad m) => (a -> a -> Bool) -> m a -> m a -> m Bool
predM p v1 v2 = p <$> v1 <*> v2

instance Applicative (ST s) where
  pure  = return
  (<*>) = ap
