module Camino.Map.Graph where

import Camino.Sparse.Graph
    ( Graph
    , sparseGraphFromMap
    )
import Camino.Strategies
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

-- | 

newtype AdjacencyMap a = AdjacencyMap
    { edges :: Map a [a]
    }
    deriving (Eq, Ord, Show)

fromAdjacencies :: Ord a => [(a, [a])] -> AdjacencyMap a
fromAdjacencies = AdjacencyMap . Map.fromListWith (<>)

-- | Construct a sparse graph from an adjacency map.

sparseGraph :: Ord a => AdjacencyMap a -> Graph a
sparseGraph (AdjacencyMap adjacencyMap) = sparseGraphFromMap adjacencyMap

-- | Unfold an adjacency map from some initial values and a successor function.

unfold :: forall a. Ord a => (a -> [a]) -> [a] -> AdjacencyMap a
unfold f start = AdjacencyMap $ bfsM visit start `evalState` mempty
    where
        visit :: a -> WriterT [a] (State (Set a)) (Map a [a])
        visit x = do
            visited <- lift get
            if x `Set.member` visited
                then pure mempty
                else do
                    let ys = f x
                    tell ys
                    lift $ modify $ Set.insert x
                    pure $ Map.singleton x ys