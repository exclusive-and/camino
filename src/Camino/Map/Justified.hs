-- | A 'Map' variant with compile-time key membership information.

module Camino.Map.Justified where

import Data.Map (Map)
import Data.Map qualified as Map

-- | A key that's proven to exist in a corresponding 'JustMap'.

newtype Key ph k = Key
    { getKey :: k
    }
    deriving (Eq, Ord, Show)

-- | A 'Map' variant with a phantom type parameter that encodes key membership information at
--   compile-time.

newtype JustMap ph k v = JustMap
    { getMap :: Map k v
    }
    deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

-- | Execute a continuation on an existentially-quantified 'JustMap'.

withJustMap :: Map k v -> (forall ph. JustMap ph k v -> r) -> r
withJustMap m cont = cont (JustMap m)

-- | Try to prove that a key exists in the input 'JustMap'.

member :: Ord k => k -> JustMap ph k v -> Maybe (Key ph k)
member k (JustMap m) = const (Key k) <$> Map.lookup k m

-- | Look up a 'Key' in a 'JustMap'. The phantom type parameter proves that the key must be in
--   the map.

lookup :: Ord k => Key ph k -> JustMap ph k v -> v
lookup (Key k) (JustMap m) = case Map.lookup k m of
    Just v  -> v
    Nothing -> error "impossible: Camino.Map.Justified has been subverted!"