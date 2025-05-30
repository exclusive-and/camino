module Camino.Identify where

import Camino.Strategies (bfsM)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Map (Map)
import Data.Map qualified as Map

-- | Identify a value with an 'Int'. Identifications made in the same context
--   are unique up to equality.
--
-- Whenever I identify a value I haven't seen before, I inform my caller of that fact by
-- 'tell'ing @(the value, its identification)@.

identify :: Ord a => a -> Identify a Int
identify x = do
    (freshId, idMap) <- lift get
    case Map.lookup x idMap of
        Just oldId -> pure oldId
        Nothing -> do
            tell [(x, freshId)]
            lift $ put $ (freshId + 1, Map.insert x freshId idMap)
            pure freshId

-- | The context monad for 'identify'.

type Identify a = WriterT [(a, Int)] (State (Int, Map a Int))

-- | Transitively identify values and their successors.

identifyBfs :: forall a. Ord a => (a -> [a]) -> [a] -> Map Int (a, [Int])
identifyBfs f start = (bfsM visit =<< initial) `evalState` (0, mempty)
    where
        visit :: (a, Int) -> Identify a (Map Int (a, [Int]))
        visit (x, v) = do
            v  <- identify x
            ws <- traverse identify (f x)
            pure $ Map.singleton v (x, ws)
        
        initial = execWriterT $ traverse identify $ start