-- | Existential quantification for your map keys, via a phantom type variable.

module Camino.Map.Justified
    ( JustMap
    , Key
    , forgetKey
    , withJustMap
    , member
    , lookup
    , mapWithKey
    , traverseWithKey
    ) where

-- See Note [Camino.Map.Justified attribution]

import Prelude hiding (lookup)

import Data.Map (Map)
import Data.Map qualified as Map

-- | A key that knows it can be found in some 'JustMap's. The key's knowledge is conveyed via
--   the phantom type @ph@.

newtype Key ph k = Key
    { getKey :: k
    }
    deriving (Eq, Ord, Show)

-- | Get a bare key, forgetting its memberships.

forgetKey :: Key ph k -> k
forgetKey = getKey

-- | A 'Map' variant that knows which keys have already been found inside it.

newtype JustMap ph k v = JustMap
    { getMap :: Map k v
    }
    deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

-- | Execute a continuation on the input map. Within the continuation, @ph@ carries information
--   about which keys are known to exist in the input map.

withJustMap :: Map k v -> (forall ph. JustMap ph k v -> r) -> r
withJustMap m cont = cont (JustMap m)

-- | Try to prove that a key exists in a 'JustMap'.

member :: Ord k => k -> JustMap ph k v -> Maybe (Key ph k)
member k (JustMap m) = const (Key k) <$> Map.lookup k m
        
-- | Look up a 'Key' in a 'JustMap'. @ph@ proves that we already know that the key is present
--   in the map.

lookup :: Ord k => Key ph k -> JustMap ph k v -> v
lookup (Key k) (JustMap m) = case Map.lookup k m of
    Just v  -> v
    Nothing -> error "impossible: Camino.Map.Justified has been subverted!"

-- | Map a function over the keys and values in a 'JustMap'.

mapWithKey :: (Key ph k -> a -> b) -> JustMap ph k a -> JustMap ph k b
mapWithKey f (JustMap m) = JustMap (Map.mapWithKey g m)
    where
        g k = f (Key k)

-- | Traverse over the keys and values in a 'JustMap'.

traverseWithKey :: Applicative f
                => (Key ph k -> a -> f b)
                -> JustMap ph k a
                -> f (JustMap ph k b)

traverseWithKey f (JustMap m) = JustMap <$> Map.traverseWithKey g m
    where
        g k = f (Key k)

{-
Note [Camino.Map.Justified attribution]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
I owe the inspiration for this module to Matt Noonan's justified-containers package:

    Noonan, M. "justified-containers".
        https://hackage.haskell.org/package/justified-containers (accessed on 2025-04-29).
-}