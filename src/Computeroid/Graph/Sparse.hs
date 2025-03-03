module Computeroid.Graph.Sparse where

import Computeroid.AdjacencyMap
import Computeroid.AdjacencyMap qualified as AdjacencyMap
import Computeroid.Identify
import Computeroid.Strategies
import Control.Monad.ST
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Map qualified as Map
import Data.Primitive.Array

-- | Sparse directed graphs.

data Graph a = Graph
    { outs  :: Array [Vertex]   -- ^ Array of outgoing edges from each vertex.
    , nodes :: Array a          -- ^ Array of the original data or name of each vertex.
    }

type Vertex = Int

-- | Construct a sparse graph from an anonymous adjacency list.
--
-- "Anonymity" in this context means that the vertices are assumed to be identified only with
-- sequential numbers, as they would be after 'identify'ing them.

fromAdjacenciesAnon :: [[Vertex]] -> Graph Vertex
fromAdjacenciesAnon adjs =
    let
        outs  = arrayFromList adjs
        nodes = arrayFromList [0..length outs]
    in
        Graph outs nodes

-- | Construct a sparse graph from an adjacency map.

fromAdjacencyMap :: forall a. Ord a => AdjacencyMap a -> Graph a
fromAdjacencyMap (AdjacencyMap adjacencyMap) =
    let
        outs  = arrayFromList $ map snd adjs
        nodes = arrayFromList $ map fst adjs
    in
        Graph outs nodes
    where
        adjs = (bfsM visit =<< initial) `evalState` (0, mempty)

        visit :: (a, Int) -> Identify a [(a, [Int])]
        visit (x, v) = do
            v  <- identify x
            ws <- traverse identify (Map.findWithDefault [] x adjacencyMap)
            pure $ [(x, ws)]
        
        initial = execWriterT $ traverse identify $ Map.keys adjacencyMap

-- | Construct a sparse graph from an adjacency list.

fromAdjacencies :: forall a. Ord a => [(a, [a])] -> Graph a
fromAdjacencies = fromAdjacencyMap . AdjacencyMap.fromAdjacencies