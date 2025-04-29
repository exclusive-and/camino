-- | Existential quantification for your map keys, via a phantom type variable.

module Camino.Map.Justified
    ( JustMap
    , Key
    , withJustMap
    , traverseWithKey
    , member
    , lookup
    ) where

import Prelude hiding (lookup)

import Data.Map (Map)
import Data.Map qualified as Map

-- | A key that knows it can be found in some 'JustMap's. The key's knowledge is conveyed via
--   the phantom type @ph@.

newtype Key ph k = Key
    { getKey :: k
    }
    deriving (Eq, Ord, Show)

-- | A 'Map' variant that knows which keys have already been found inside it.

newtype JustMap ph k v = JustMap
    { getMap :: Map k v
    }
    deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

-- | Execute a continuation on the input map. Within the continuation, @ph@ carries information
--   about which keys are known to exist in the input map.

withJustMap :: Map k v -> (forall ph. JustMap ph k v -> r) -> r
withJustMap m cont = cont (JustMap m)

-- | Traverse a 'JustMap' with its 'Key's visible to the traversing function.

traverseWithKey :: Applicative f
                => (Key ph k -> a -> f b)
                -> JustMap ph k a
                -> f (JustMap ph k b)

traverseWithKey f (JustMap m) = JustMap <$> Map.traverseWithKey g m
    where
        g k = f (Key k)

-- | Try to prove that a key exists in the input 'JustMap'.

member :: Ord k => k -> JustMap ph k v -> Maybe (Key ph k)
member k (JustMap m) = const (Key k) <$> Map.lookup k m
        
-- | Look up a 'Key' in a 'JustMap'. @ph@ proves that we already know that the key is present
--   in the map.

lookup :: Ord k => Key ph k -> JustMap ph k v -> v
lookup (Key k) (JustMap m) = case Map.lookup k m of
    Just v  -> v
    Nothing -> error "impossible: Camino.Map.Justified has been subverted!"
