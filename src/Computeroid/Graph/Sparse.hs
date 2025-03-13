module Computeroid.Graph.Sparse where

import Prelude hiding (map)

import Computeroid.Identify
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
        outs'  = arrayFromList outs
        nodes' = arrayFromList nodes
    in
        Graph outs' nodes'
    where
        (nodes, outs) =
            unzip $ Map.elems
                  $ identifyBfs (\x -> Map.findWithDefault [] x adjacencyMap)
                  $ Map.keys adjacencyMap

-- | Construct a sparse graph from an adjacency list.

fromAdjacencies :: forall a. Ord a => [(a, [a])] -> Graph a
fromAdjacencies = sparseGraphFromMap . Map.fromListWith (<>)