-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Linear
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- This package contains efficient implementations of various generic
-- graph traversals, including breadth- and depth- first search, computation
-- of strongly connected components,  bi-connected components and edge
-- classification. Each operation is implemented as a state-transformer,
-- ensuring efficient linear space and runtime execution (bounded within the
-- size of the graph).
--
-----------------------------------------------------------------------------
--
module Data.Graph.Linear where

import Data.Graph.Linear.Graph
import Data.Graph.Linear.Basic
import Data.Graph.Linear.Query.DFS
import Data.Graph.Linear.Query.BCC
import Data.Graph.Linear.Query.SCC
