module Camino.Sparse.Graph where

import Camino.Identify
import Control.Monad.ST
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Foldable
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Primitive.Array

-- | Sparse directed graphs.

data Graph a = Graph
    { edges :: Array [Vertex]   -- ^ Array of outgoing edges from each vertex.
    , nodes :: Array a          -- ^ Array of the original data or name of each vertex.
    }
    deriving (Eq, Show)

type Vertex = Int

-- | Map the vertices in a graph, without affecting its overall structure.

instance Functor Graph where
    fmap f Graph{edges, nodes} = Graph edges (fmap f nodes)
    
-- | Construct a sparse graph from adjacency lists of numbered vertices.

unsafeMakeGraph :: [[Vertex]] -> Graph Vertex
unsafeMakeGraph adjs =
    let
        edges = arrayFromList adjs
        nodes = arrayFromList [0..length edges - 1]
    in
        Graph {edges, nodes}

-- | Construct a sparse graph from an adjacency map. Mainly intended for internal use.

fromMap :: forall a. Ord a => Map a [a] -> Graph a
fromMap adjacencyMap =
    let
        (nodes, edges) = unzip $ Map.elems $ identifyBfs go $ Map.keys adjacencyMap
    in
        Graph  
            { edges = arrayFromList edges
            , nodes = arrayFromList nodes
            }
    where
        go x = Map.findWithDefault [] x adjacencyMap

-- | A problem raised by 'fromMapExact' when it encounters a vertex with no corresponding key.

newtype CantFindKeyForVertex a = CantFindKeyForVertex a
    deriving (Eq, Ord, Show)

-- | Construct a sparse graph whose vertices exactly match the keys of the input adjacency map.

fromMapExact :: forall a. Ord a => Map a [a] -> Either [CantFindKeyForVertex a] (Graph a)
fromMapExact input = case problems of
    [] -> Right graph
    _  -> Left problems
    where
        graph = runST $ do
            nodes <- newArray (length converted) undefined
            edges <- newArray (length converted) []
            let f k (v, ws) = do
                    writeArray nodes v k
                    writeArray edges v ws
            _ <- Map.traverseWithKey f converted
            Graph <$> unsafeFreezeArray edges <*> unsafeFreezeArray nodes

        (converted, problems) = runWriter $ traverse convertEdges enumerated

        convertEdges (n, ys) = fmap (n,) $ fmap catMaybes $ traverse convertEdge ys

        convertEdge y = case Map.lookup y enumerated of
            Nothing     -> tell [CantFindKeyForVertex y] >> pure Nothing
            Just (w, _) -> pure $ Just w

        enumerated = traverse enumerate input `evalState` 0

        enumerate x = get >>= \n -> put (n + 1) >> pure (n, x)

-- | Construct a sparse graph from an adjacency list.

fromAdjacencies :: forall a. Ord a => [(a, [a])] -> Graph a
fromAdjacencies = fromMap . Map.fromListWith (<>)

-- | Create a new graph with only the raw internal structure of the input graph.

structure :: Graph a -> Graph Vertex
structure Graph{edges} = Graph edges (arrayFromList [0..length edges - 1])

-- | A function that rebuilds the internal structure of a graph.

newtype GraphRebuild = GraphRebuild
    { runRebuild :: forall s.
        Vertex -> [Vertex] -> ReaderT (MutableArray s [Vertex]) (ST s) ()
    }

-- | Modify the internal structure of a graph. Doesn't touch nodes at all.

rebuildStructure :: GraphRebuild -> Graph a -> Graph a
rebuildStructure f input =
    let
        Graph{edges, nodes} = structure input

        go = runArray $ do
            edges' <- newArray (length edges) []
            let g v = runRebuild f v (edges `indexArray` v)
            traverse_ g nodes `runReaderT` edges'
            pure edges'
    in
        Graph go input.nodes