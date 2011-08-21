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
(
  Graph, mkGraphFromVerticesAndAdjacency, mkGraphFromEdgedVertices,
)
where

import 
