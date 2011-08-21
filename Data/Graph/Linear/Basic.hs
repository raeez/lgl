-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Linear.Basic
-- Copyright   :  (c) The University of Glasgow 2002
--                (c) The University of Glasgow 2006
--                (c) Raeez Lorgat 2011
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Data.Graph.Linear.Basic implements various convenience constructors for
-- linear time and space graph representations, as well as a set of basic graph
-- algorithms.
--
-----------------------------------------------------------------------------
--
module Data.Graph.Linear.Basic
(
  Graph, mkGraphFromVerticesAndAdjacency, mkGraphFromEdgedVertices,
)
where

import 
