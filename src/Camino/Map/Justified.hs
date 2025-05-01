-- | Justify map operations ahead of time by remembering which keys were already found.

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

-- | A key that knows it can be found in some 'JustMap's.

newtype Key ph k = Key
    { getKey :: k
    }
    deriving (Eq, Ord, Show)

-- | Get a bare key, forgetting its memberships.

forgetKey :: Key ph k -> k
forgetKey = getKey

-- | A 'Map' variant that knows which keys are known members.

newtype JustMap ph k v = JustMap
    { getMap :: Map k v
    }
    deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

-- | Execute a continuation on the input map. @ph@ will remember any keys found within
--   the scope of the continuation.

withJustMap :: Map k v -> (forall ph. JustMap ph k v -> r) -> r
withJustMap m cont = cont (JustMap m)

-- | Try to find a key in a 'JustMap'. If the key lookup succeeds, then @ph@ remembers it.

member :: Ord k => k -> JustMap ph k v -> Maybe (Key ph k)
member k (JustMap m) = const (Key k) <$> Map.lookup k m

-- | Look up the value at a key, with justification from @ph@.

lookup :: Ord k => Key ph k -> JustMap ph k v -> v
lookup (Key k) (JustMap m) = case Map.lookup k m of
    Just v  -> v
    Nothing -> error "impossible: Camino.Map.Justified has been subverted!"

-- | Internal: coerce a function on 'Map's into one on 'JustMap's.

coerceMap   :: (Map kx x -> Map ky y)
            -> JustMap ph kx x
            -> JustMap ph' ky y

coerceMap f (JustMap m) = JustMap (f m)

-- | Internal: 'coerceMap' for functions with functorial results.

coerceFmap  :: Functor f
            => (Map kx x -> f (Map ky y))
            -> JustMap ph kx x
            -> f (JustMap ph' ky y)

coerceFmap f (JustMap m) = JustMap <$> f m

-- | Internal: coerce a function into thinking that @ph@ remembers @k@.

coerceKey :: (Key ph k -> r) -> k -> r
coerceKey f = f . Key

-- | Map over the keys and values in a 'JustMap'.

mapWithKey :: (Key ph k -> a -> b) -> JustMap ph k a -> JustMap ph k b
mapWithKey f = coerceMap (Map.mapWithKey $ coerceKey f)

-- | Traverse over the keys and values in a 'JustMap'.

traverseWithKey :: Applicative f
                => (Key ph k -> a -> f b)
                -> JustMap ph k a
                -> f (JustMap ph k b)

traverseWithKey f = coerceFmap (Map.traverseWithKey $ coerceKey f)

{-
Note [Camino.Map.Justified attribution]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
I owe the inspiration for this module to Matt Noonan's justified-containers package:

    Noonan, M. "justified-containers".
        https://hackage.haskell.org/package/justified-containers (accessed on 2025-04-29).
-}