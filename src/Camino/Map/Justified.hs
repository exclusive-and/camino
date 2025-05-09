module Camino.Map.Justified
    (
    -- * Description
    
    -- $ModuleDescription
    
    -- * Justified maps
    
      JustMap
    , withJustMap

    -- * Justified queries

    -- ** Keys
    , Key
    , forgetKey
    , (?)
    , member

    -- ** Lookup
    , (!)
    , lookup

    -- ** Example
    -- $JustifiedQueryExample

    -- * Traversal

    , mapWithKey
    , traverseWithKey
    ) where

import Prelude hiding (lookup)

import Data.Map (Map)
import Data.Map qualified as Map

{- $ModuleDescription

Map queries generally involve determining whether a key is present or not, either directly
or indirectly. Handling these cases can result in cluttered code; especially in
situations where we already know that certain keys /must/ be present.

"Camino.Map.Justified" defines a map interface based on keys that have /intrinsic membership/:
for some phantom type @ph@, a key @'Key' ph k@ is guaranteed to be a member of all maps
@'JustMap' ph k v@ in the same scope. Consequently, the presence of these keys is a given for
query operations.

In short: 'lookup' returns only @v@; not @'Maybe' v@! This shortcut is justified by the key's
intrinsic membership.

= Attribution

This module is inspired by Matt Noonan's [@justified-containers@](https://hackage.haskell.org/package/justified-containers).
-}

-- | A map from keys @'Key' ph k@ to values @v@.

newtype JustMap ph k v = JustMap
    { getMap :: Map k v
    }
    deriving (Eq, Ord, Show)
    deriving stock (Foldable, Functor, Traversable)

type role JustMap phantom nominal representational

-- | Execute a continuation on the input map. @ph@ will remember any keys found within
--   the scope of the continuation.

withJustMap :: Map k v -> (forall ph. JustMap ph k v -> r) -> r
withJustMap m cont = cont (JustMap m)

-- | A key that is intrinsically a member of maps @'JustMap' ph k v@.

newtype Key ph k = Key
    { getKey :: k
    }
    deriving (Eq, Ord, Show)

type role Key phantom representational

-- | Get a bare key, forgetting its memberships.

forgetKey :: Key ph k -> k
forgetKey = getKey

-- | @someMap '?' key@ is shorthand for @'member' key someMap@.

(?) :: Ord k => JustMap ph k v -> k -> Maybe (Key ph k)
(?) = flip member

-- | Try to find a key in a 'JustMap'. If the key lookup succeeds, then @ph@ remembers it.

member :: Ord k => k -> JustMap ph k v -> Maybe (Key ph k)
member k (JustMap m) = const (Key k) <$> Map.lookup k m

-- | @someMap '!' key@ is shorthand for @'lookup' key someMap@.

(!) :: Ord k => JustMap ph k v -> Key ph k -> v
(!) = flip lookup

-- | Look up the value at a key, with justification from @ph@.

lookup :: Ord k => Key ph k -> JustMap ph k v -> v
lookup (Key k) (JustMap m) = case Map.lookup k m of
    Just v  -> v
    Nothing -> error "impossible: Camino.Map.Justified has been subverted!"

{- $JustifiedQueryExample

Here's an example program that demonstrates how 'member' justifies 'lookup' operations
within the appropriate scope at compile-time:

@
    let sample = Map.fromList [(1, "hello"), (2, "world"), (3, "!")]

    'withJustMap' sample $ \\input -> do

    case 1 \`'member'\` input of

        Nothing -> putStrLn "I couldn\'t prove that key 1 is present."

        Just justified1 -> do

        -- I\'ve proven that the key is present.
        -- Now I can use it for free, without incurring any additional checks.

        putStrLn $ "Found " ++ 'lookup' justified1 input ++ " at key " ++ show justified1

        case 2 \`'member'\` input of

          Nothing -> putStrLn "I couldn\'t prove that key 2 is present."

          Just justified2 -> do

            -- In here I can use both justified1 and justified2 freely!

            putStrLn $ 'lookup' justified1 input ++ " " ++ 'lookup' justified2 input
@

Output:

@
    Found hello at key "1"
    hello world
@
-}

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
