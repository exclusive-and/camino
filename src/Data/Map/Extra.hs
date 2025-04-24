module Data.Map.Extra where

import Control.Monad.Trans.State
import Data.Map (Map)
import Data.Map qualified as Map

-- | Assign a unique number to each value in a map.

enumerate :: Ord k => Map k v -> Map k (Int, v)
enumerate input = traverse go input `evalState` 0
    where
        go x = get >>= \n -> put (n + 1) >> pure (n, x)