module Camino.Graph.Sparse where

import Prelude hiding (map)

import Camino.Identify
import Data.Foldable
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Primitive.Array
import Data.Set qualified as Set

-- | Sparse directed graphs.

data Graph a = Graph
    { edges :: Array [Vertex]   -- ^ Array of outgoing edges from each vertex.
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

-- | Create a new graph with only the raw internal structure of the input graph.

structure :: Graph a -> Graph Vertex
structure Graph{edges} = Graph edges (arrayFromList [0..length edges])

-- | Map the vertices in a graph, without affecting its overall structure.

map :: (a -> b) -> Graph a -> Graph b
map f (Graph outs nodes) = Graph outs (fmap f nodes)

-- | Construct a sparse graph from an adjacency map. Mainly intended for internal use.

sparseGraphFromMap :: forall a. Ord a => Map a [a] -> Graph a
sparseGraphFromMap adjacencyMap =
    let
        edges' = arrayFromList edges
        nodes' = arrayFromList nodes
    in
        Graph edges' nodes'
    where
        (nodes, edges) =
            unzip $ Map.elems
                  $ identifyBfs (\x -> Map.findWithDefault [] x adjacencyMap)
                  $ Map.keys adjacencyMap

-- | Construct a sparse graph from an adjacency list.

fromAdjacencies :: forall a. Ord a => [(a, [a])] -> Graph a
fromAdjacencies = sparseGraphFromMap . Map.fromListWith (<>)

-- | Compute the /reflexive closure/ of a graph.

reflexive :: Graph a -> Graph a
reflexive Graph{edges, nodes} =
    let
        edges' = createArray (length edges) [] $ go (length edges)
    in
        Graph edges' nodes
    where
        go n edges' =
            if n <= 0 then
                pure ()
            else do
                let ws  = edges `indexArray` n
                    ws' = Set.toList $ Set.fromList (n:ws)
                writeArray edges' n ws'
                go (n - 1) edges'