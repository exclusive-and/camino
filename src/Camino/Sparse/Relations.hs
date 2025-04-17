module Camino.Sparse.Relations where

import Camino.Sparse.Graph
import Camino.Sparse.Reaching (SCC (..), reachingSets)
import Control.Monad.ST
import Control.Monad.Trans.Reader
import Data.Foldable
import Data.Primitive.Array
import Data.Set (Set)
import Data.Set qualified as Set

-- | Create a new graph whose edges all face in the opposite direction of their original
--   counterpart in the input graph.

opposite :: Graph a -> Graph a
opposite = rebuildStructure $ GraphRebuild go
    where
        go v ws = traverse_ (invert v) ws

        invert :: Vertex -> Vertex -> ReaderT (MutableArray s [Vertex]) (ST s) ()
        invert v w = do
            arr <- ask
            vs <- arr `readArray` w
            writeArray arr w (v:vs)

-- | Compute the /reflexive closure/ of a graph.

reflexive :: Graph a -> Graph a
reflexive = rebuildStructure $ GraphRebuild go
    where
        go :: Vertex -> [Vertex] -> ReaderT (MutableArray s [Vertex]) (ST s) ()
        go v ws = do
            arr <- ask
            let ws' = Set.toList $ Set.fromList (v:ws)
            writeArray arr v ws'

-- | Compute the /reflexive transitive closure/ of a graph.

transitive :: Graph a -> Graph a
transitive input@Graph{nodes} =
    let
        sccs = reachingSets (structure input)
    in
        Graph (go sccs) nodes
    where
        go sccs = runArray $ do
            edges' <- newArray (length nodes) []
            traverse_ propagate sccs `runReaderT` edges'
            pure edges'

        propagate (Trivial rs v ) = do
            edges' <- ask
            writeArray edges' v $ Set.toList rs

        propagate (Cycle rs vs) = do
            edges' <- ask
            let rs' = Set.toList rs
            traverse_ (\v -> writeArray edges' v rs') vs