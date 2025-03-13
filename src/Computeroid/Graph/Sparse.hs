module Computeroid.Graph.Sparse where

import Prelude hiding (map)

import Computeroid.Identify
import Computeroid.Strategies
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Primitive.Array

-- | Sparse directed graphs.

data Graph a = Graph
    { outs  :: Array [Vertex]   -- ^ Array of outgoing edges from each vertex.
    , nodes :: Array a          -- ^ Array of the original data or name of each vertex.
    }
    deriving (Eq, Show)

type Vertex = Int

instance Functor Graph where
    fmap = map

-- | Construct a sparse graph from adjacency lists of numbered vertices.

unsafeFromVertices :: [[Vertex]] -> Graph Vertex
unsafeFromVertices adjs =
    let
        outs  = arrayFromList adjs
        nodes = arrayFromList [0..length outs]
    in
        Graph outs nodes

-- | Map the vertices in a graph, without affecting its overall structure.

map :: (a -> b) -> Graph a -> Graph b
map f (Graph outs nodes) = Graph outs (fmap f nodes)

-- | Construct a sparse graph from an adjacency map. Mainly intended for internal use.

sparseGraphFromMap :: forall a. Ord a => Map a [a] -> Graph a
sparseGraphFromMap adjacencyMap =
    let
        outs  = arrayFromList $ fmap snd adjs
        nodes = arrayFromList $ fmap fst adjs
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
fromAdjacencies = sparseGraphFromMap . Map.fromListWith (<>)