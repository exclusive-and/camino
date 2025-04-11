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
            ns <- newArray (length g) (-1)
            ys <- newArray (length g) mempty
            sccfold ([], [], 0) [0..(length g - 1)] `runReaderT` (ns, ys)
    in
        sccs
    where
        sccfold = foldrM (whenInteresting go)

        whenInteresting f v s = do
            (ns, _) <- ask
            n <- ns `readArray` v
            if n < 0 then f v s else pure s
        
        go  :: Vertex
            -> ([SCC b], [Vertex], Int)
            -> ReaderT  (MutableArray s Int, MutableArray s b)
                        (ST s)
                        ([SCC b], [Vertex], Int)
        
        -- This implementation was derived from the outline of the @Digraph@ algorithm given
        -- in https://doi.org/10.1145/69622.357187.
        --
        -- Both my implementation and the one in the paper are variants of Tarjan's algorithm.

        go v (sccs, stack, depth) = do
            (ns, ys) <- ask
            -- 1. Compute initial preorder number.
            writeArray ns v depth
            -- 2. Recurse on adjacent vertices.
            let ws = g `indexArray` v
            (sccs', stack', depth') <- sccfold (sccs, v:stack, depth + 1) ws
            -- 3. Compute new preorder.
            ns' <- traverse (readArray ns) ws
            let n' = foldr min depth ns'
            writeArray ns v n'
            -- 4. Compute immediate and combined results.
            --    See Note [Recurse before calculating the immediate result].
            let y = f (xs `indexArray` v)
            ys' <- traverse (readArray ys) ws
            let y' = mconcat (y:ys')
            writeArray ys v y'
            -- 5. Create a new SCC if one is detected.
            if n' == depth
                then popScc [] v (sccs', stack', depth')
                else pure (sccs', stack', depth')
        
        popScc scc v (sccs, []     , _depth) = pure (sccs, [], 0)
        popScc scc v (sccs, x:stack,  depth) = do
            (ns, ys) <- ask
            writeArray ns x maxBound
            let depth' = depth - 1
            if v == x then do
                y <- ys `readArray` x
                -- Need to make sure that every vertex in the SCC gets the same final result!
                forM_ scc $ \v -> writeArray ys v y
                pure (createScc scc x y : sccs, stack, depth')
            else
                popScc (x:scc) v (sccs, stack, depth')
        
        createScc []  x y | x `notElem` (g `indexArray` x) = Trivial y x
        createScc scc x y = Cycle y (x :| scc)

{-
Note [Recurse before calculating the immediate result]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Contrary to the algorithm in the paper, my 'contractMap' implementation recurses /before/
computing the immediate result of a vertex. This change is important, because it
means 'contractMap' won't allow a vertex to contribute to the result of its own SCC twice.
-}

-- | Compute the strongly connected components of a graph.

sccs :: Graph a -> [SCC ()]
sccs = contractMap (const ())

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
-- [Trivial [A,B,C,C] 0,Trivial [B,C] 1,Trivial [C] 2]
--
-- Notice that multiple copies of @C@ show up? The intuitive reason is that the number of
-- copies of @C@ equals the total number of distinct paths through the graph that include @C@.
--
-- However, we don't see repetition in a cycle:
--
-- >>> reachingMulti $ fromAdjacencies [(A, [B]), (B, [A])]
-- [Cycle [B,A] (1 :| [0])]
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
condensation input@Graph{edges} =
    let
        sccs = zip [0..] $ contractMap id input
        labels = labelSccs sccs
        (edges', nodes) = unzip $ mapScc labels <$> sccs
    in
        Graph (arrayFromList edges') (arrayFromList nodes)
    where
        mapScc :: Array Vertex -> (Int, SCC a) -> ([Vertex], SCC a)
        mapScc labels (n, scc) = (mapEdges labels n scc, scc)

        mapEdges _labels _ (Trivial _ v ) = indexArray edges v
        mapEdges  labels n (Cycle   _ vs) =
            filter (/= n) $ fmap (indexArray labels) $ concatMap (indexArray edges) vs

        labelSccs :: [(Vertex, SCC a)] -> Array Vertex
        labelSccs sccs =
            createArray (length input.nodes) (-1) $
                \labels -> traverse_ (uncurry $ labelScc labels) sccs

        labelScc labels n = traverse_ (\v -> writeArray labels v n) . sccVerts

        sccVerts (Trivial _ v ) = [v]
        sccVerts (Cycle   _ vs) = NE.toList vs