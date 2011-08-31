module Data.Graph.Linear.Query.Util where

import Control.Applicative
import Control.Monad(ap)
import Control.Monad.ST

import Data.Graph.Linear.Graph
import Data.Graph.Linear.Representation.Array

unvisited :: STMapping s Int -> Vertex -> ST s Bool
unvisited marks w = readSTMap marks w .==. return 0

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p tb fb = p >>= \pred -> if pred then tb else fb

whenM :: Monad m => m Bool -> m a -> m ()
whenM p tb = p >>= \pred -> if pred then tb >> return () else return ()

(.==.) :: (Applicative m, Monad m, Eq a) => m a -> m a -> m Bool
(.==.) = predM (==)

(./=.) :: (Applicative m, Monad m, Eq a) => m a -> m a -> m Bool
(./=.) = predM (/=)

(.<.) :: (Applicative m, Monad m, Ord a) => m a -> m a -> m Bool
(.<.) = predM (<)

(.>.) :: (Applicative m, Monad m, Ord a) => m a -> m a -> m Bool
(.>.) = predM (>)

(.>=.) :: (Applicative m, Monad m, Ord a) => m a -> m a -> m Bool
(.>=.) = predM (>=)

(.&&.) ::  (Applicative m, Monad m) => m Bool -> m Bool -> m Bool
(.&&.) = predM (&&)

predM ::  (Applicative m, Monad m) => (a -> a -> Bool) -> m a -> m a -> m Bool
predM p v1 v2 = p <$> v1 <*> v2

instance Applicative (ST s) where
  pure  = return
  (<*>) = ap
