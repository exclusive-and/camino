module Camino.Sparse.Graph where

import Prelude hiding (map)

import Camino.Identify
import Control.Monad.ST
import Control.Monad.Trans.Reader
import Data.Foldable
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Primitive.Array

-- | Sparse directed graphs.

data Graph a = Graph
    { edges :: Array [Vertex]   -- ^ Array of outgoing edges from each vertex.
    , nodes :: Array a          -- ^ Array of the original data or name of each vertex.
    }
    deriving (Eq, Show)

type Vertex = Int

instance Functor Graph where
    fmap = map

-- | Map the vertices in a graph, without affecting its overall structure.

map :: (a -> b) -> Graph a -> Graph b
map f Graph{edges, nodes} = Graph edges (fmap f nodes)

-- | Create a new graph with only the raw internal structure of the input graph.

structure :: Graph a -> Graph Vertex
structure Graph{edges} = Graph edges (arrayFromList [0..length edges - 1])

-- | Modify the internal structure of a graph. Doesn't touch nodes at all.

traverseStructure ::
       (forall s. Vertex -> [Vertex] -> ReaderT (MutableArray s [Vertex]) (ST s) ())
    -> Graph a
    -> Graph a

traverseStructure f input =
    let
        Graph{edges, nodes} = structure input

        go = runArray $ do
            edges' <- newArray (length edges) []
            let g v = f v (edges `indexArray` v)
            traverse_ g nodes `runReaderT` edges'
            pure edges'
    in
        Graph go input.nodes
    
-- | Construct a sparse graph from adjacency lists of numbered vertices.

unsafeFromVertices :: [[Vertex]] -> Graph Vertex
unsafeFromVertices adjs =
    let
        edges = arrayFromList adjs
        nodes = arrayFromList [0..length edges - 1]
    in
        Graph edges nodes

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