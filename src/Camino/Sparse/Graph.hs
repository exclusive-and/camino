module Camino.Sparse.Graph where

import Camino.Identify
import Camino.Map.Justified.Graph
    ( JustGraph (..)
    , JustGraphProblem (..)
    , withJustGraph
    )
import Camino.Map.Justified (JustMap, Key)
import Camino.Map.Justified qualified as JustMap
import Control.Monad.ST
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Bifunctor (second)
import Data.Foldable
import Data.Functor (void)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.Extra qualified as Map
import Data.Maybe (catMaybes)
import Data.Primitive.Array
import Data.Set (Set)
import Data.Set qualified as Set

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

-- | Construct a sparse graph from a 'JustGraph'.

fromJustGraph :: Ord a => JustGraph ph a -> Graph a
fromJustGraph (JustGraph jg) = 
    runST $ do
        edges <- newArray size (error "impossible")
        nodes <- newArray size (error "impossible")
        build edges nodes edgesMap
        Graph <$> unsafeFreezeArray edges <*> unsafeFreezeArray nodes
    where
        (edgesMap, size) = traverse sparsify jg `runState` 0

        sparsify ks = do
            n <- get
            put (n + 1)
            pure (n, map (\k -> fst $ JustMap.lookup k edgesMap) ks)
        
        build edges nodes =
            let
                f kx (n, ws) = do
                    writeArray edges n ws
                    writeArray nodes n (JustMap.forgetKey kx)
            in
                void . JustMap.traverseWithKey f

-- | Try to construct a sparse graph from a map. Fails with an exception if the map cannot
--   be converted to a 'JustGraph'.

fromMapExact :: (Ord a, Monad m) => Map a [a] -> ExceptT (JustGraphProblem a) m (Graph a)
fromMapExact input = withJustGraph input fromJustGraph

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