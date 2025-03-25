-- | Some algorithms for reaching analysis of graphs, centered around 'contractMap'.

module Camino.Reaching where

import Camino.Graph.Sparse
import Control.Monad.ST
import Control.Monad.Trans.Reader
import Data.Foldable
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Primitive.Array
import Data.Set (Set)
import Data.Set qualified as Set

-- | A strongly connected component. Within the same 'SCC', every vertex can always reach any
--   other vertex.

data SCC a = Trivial a Vertex | Cycle a (NonEmpty Vertex)
    deriving (Eq, Show)

instance Functor SCC where
    fmap f (Trivial x v) = Trivial (f x) v
    fmap f (Cycle   x v) = Cycle   (f x) v

-- | Compute the strongly connected components of a graph.

sccs :: Graph a -> [SCC ()]
sccs = contractMap (const ())

-- | \(O(V + E)\). Map the vertices in a graph into a monoid, and contract along its edges
--   with @('<>')@. Outputs the strongly connected components of the graph because,
--   within the same 'SCC', the contraction is the same (up to shuffling) for all vertices.
--
-- The algorithm assumes that @('<>')@ is somewhat commutative, since the order of contractions
-- depends on the hidden structure of the input graph.

contractMap :: forall b a. Monoid b => (a -> b) -> Graph a -> [SCC b]
contractMap f (Graph g xs) =
    let
        (sccs, _stack, _depth) = runST $ do
            ns <- newArray (length g) 0
            ys <- newArray (length g) mempty
            sccfold ([], [], 0) [0..(length g - 1)] `runReaderT` (ns, ys)
    in
        sccs
    where
        sccfold = foldrM (whenInteresting go)

        whenInteresting f v s = do
            (ns, _) <- ask
            n <- ns `readArray` v
            if n == 0 then f v s else pure s
        
        -- This implementation was derived from the outline of the @Digraph@ algorithm given
        -- in https://doi.org/10.1145/69622.357187.
        --
        -- Both my implementation and the one in the paper are variants of Tarjan's algorithm.
        
        go  :: Vertex
            -> ([SCC b], [Vertex], Int)
            -> ReaderT  (MutableArray s Int, MutableArray s b)
                        (ST s)
                        ([SCC b], [Vertex], Int)
        
        go v (sccs, stack, depth0) = do
            (ns, ys) <- ask
            -- 1. Compute initial preorder number.
            let depth = depth0 + 1
            writeArray ns v depth
            -- 2. Recurse on adjacent vertices.
            let ws = g `indexArray` v
            (sccs', stack', depth') <- sccfold (sccs, v:stack, depth) ws
            -- 3. Compute new preorder.
            ns' <- traverse (readArray ns) ws
            let n' = foldr min depth ns'
            writeArray ns v n'
            -- 4. Compute immediate and combined results.
            --    See Note [Recurse before calculating the immediate result].
            let y = f (xs `indexArray` v)
            ys' <- traverse (readArray ys) ws
            let y' = foldr (<>) y ys'
            writeArray ys v y'
            -- 5. Create a new SCC if one is detected.
            if n' == depth
                then consScc [] v (sccs', stack', depth')
                else pure (sccs', stack', depth')
        
        consScc scc v (sccs, [], _) = pure (sccs, [], 0)
        
        consScc scc v (sccs, x:stack, depth) = do
            (ns, ys) <- ask
            writeArray ns x maxBound
            let depth' = depth - 1
            if v == x
                then do
                    y <- ys `readArray` x
                    if  | not (x `elem` g `indexArray` x)
                        , null scc  -> pure (Trivial y x : sccs, stack, depth')
                        | otherwise -> pure (Cycle y (x :| scc) : sccs, stack, depth')
                else consScc (x:scc) v (sccs, stack, depth' - 1)

{-
Note [Recurse before calculating the immediate result]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Contrary to the algorithm in the paper, my 'contractMap' implementation recurses /before/
computing the immediate result of a vertex. This change is important, because it
means 'contractMap' won't allow a vertex to contribute to the result of its own SCC twice.
-}

-- | Compute the reaching sets of a graph.
--
-- ==== __Examples__
--
-- Here's a simple example where we compute the reaching sets of a small graph:
--
-- >>> data ABC = A | B | C deriving (Eq, Ord, Show)
-- >>>
-- >>> reachingSets $ fromAdjacencies [(A, [B, C]), (B, [C]), (C, [])]
-- [Trivial (fromList [A,B,C]) 0,Trivial (fromList [B,C]) 1,Trivial (fromList [C]) 2]

reachingSets :: Ord a => Graph a -> [SCC (Set a)]
reachingSets = contractMap (Set.singleton)

-- | Like 'reachingSets' with repetition.
--
-- ==== __Examples__
--
-- Here's what happens when we run this function on the same simple graph that we used
-- in the 'reachingSets' example:
--
-- >>> data ABC = A | B | C deriving (Eq, Ord, Show)
-- >>>
-- >>> reachingMulti $ fromAdjacencies [(A, [B, C]), (B, [C]), (C, [])]
-- [Trivial [C,B,C,A] 0,Trivial [C,B] 1,Trivial [C] 2]
--
-- Notice that multiple copies of @C@ show up? The intuitive reason is that the number of
-- copies of @C@ equals the total number of distinct paths through the graph that include @C@.
--
-- However, we don't see repetition in a cycle:
--
-- >>> reachingMulti $ fromAdjacencies [(A, [B]), (B, [A])]
-- [Cycle [A,B] (1 :| [0])]
--
-- This exception guarantees that we are indeed only counting /paths/; i.e. walks without
-- repeated vertices.

reachingMulti :: Graph a -> [SCC [a]]
reachingMulti = contractMap (\x -> [x])

-- | Compute the /condensation/ of a graph.
--
-- The condensation of a graph is a new graph whose vertices each represent an entire 'SCC'
-- in the input graph.

condensation :: Monoid a => Graph a -> Graph (SCC a)
condensation g0@(Graph edges0 nodes0) =
    let
        sccs = contractMap id g0

        nodes1 = createArray (length nodes0) (-1) $ \nodes -> do
            forM_ (zip [0..] sccs) $ \(n, scc) -> do
                forM_ (sccVerts scc) $ \v -> writeArray nodes v n

        (edges2, nodes2) = unzip $ fmap (\(n, scc) -> (go nodes1 n scc, scc)) $ zip [0..] sccs
    in
        Graph (arrayFromList edges2) (arrayFromList nodes2)
    where
        sccVerts (Trivial _ v ) = [v]
        sccVerts (Cycle   _ vs) = NE.toList vs

        go _nodes1 _ (Trivial _ v ) = indexArray edges0 v
        go  nodes1 n (Cycle   _ vs) =
            filter (/= n) $ fmap (indexArray nodes1) $ concatMap (indexArray edges0) vs
