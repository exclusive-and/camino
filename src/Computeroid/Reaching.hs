module Computeroid.Reaching where

import Computeroid.Graph.Sparse
import Control.Monad.ST
import Control.Monad.Trans.Reader
import Data.Foldable
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Primitive.Array
import Data.Set (Set)
import Data.Set qualified as Set

-- | Strongly connected component.

data SCC a = Trivial a Vertex | Cycle a (NonEmpty Vertex)
    deriving (Eq, Show)

instance Functor SCC where
    fmap f (Trivial x v) = Trivial (f x) v
    fmap f (Cycle   x v) = Cycle   (f x) v

-- | \(O(V + E)\). Map vertices into a monoid, and combine along edges with @('<>')@.

sccmap :: forall b a. Monoid b => (a -> b) -> Graph a -> [SCC b]

sccmap f (Graph g xs) =
    let
        (sccs, _stack) = runST $ do
            ns <- newArray (length g) 0
            ys <- newArray (length g) mempty
            sccfold ([], []) [0..(length g - 1)] `runReaderT` (ns, ys)
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
        
        go  :: Vertex
            -> ([SCC b], [Vertex])
            -> ReaderT  (MutableArray s Int, MutableArray s b)
                        (ST s)
                        ([SCC b], [Vertex])
        
        go v (sccs, stack) = do
            (ns, ys) <- ask
            -- 1. Compute initial preorder number.
            let depth = length stack + 1
            writeArray ns v depth
            -- 2. Recurse on adjacent vertices.
            let ws = g `indexArray` v
            (sccs', stack') <- sccfold (sccs, v:stack) ws
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
                then consScc [] v (sccs', stack')
                else pure (sccs', stack')
        
        consScc scc v (sccs, []) = pure (sccs, [])
        
        consScc scc v (sccs, x:stack) = do
            (ns, ys) <- ask
            writeArray ns x maxBound
            if v == x
                then do
                    y <- ys `readArray` x
                    if  | not (x `elem` g `indexArray` x)
                        , null scc  -> pure (Trivial y x : sccs, stack)
                        | otherwise -> pure (Cycle y (x :| scc) : sccs, stack)
                else consScc (x:scc) v (sccs, stack)

{-
Note [Recurse before calculating the immediate result]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Contrary to the algorithm in the paper, my 'sccmap' implementation recurses /before/
computing the immediate result of a vertex. This change is important, because it
means 'sccmap' won't allow a vertex to contribute to the result of its own SCC twice.
-}

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
reachingMulti = sccmap (\x -> [x])

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
reachingSets = sccmap (Set.singleton)
