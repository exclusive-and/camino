module Data.Map.Extra where

import Control.Monad.ST
import Control.Monad.Trans.State
import Data.Foldable
import Data.Functor (void)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Primitive.Array

-- | Assign a unique number to each value in a map.

enumerate :: Ord k => Map k v -> Map k (Int, v)
enumerate input = traverse go input `evalState` 0
    where
        go x = get >>= \n -> put (n + 1) >> pure (n, x)

-- | Add indirection to a map. Outputs an array containing the values of the input map, and
--   a new map of indices into that array.
--
-- ==== __Examples__
--
-- The following example shows how to recover a value from an indirect map:
--
-- >>> data ABC = A | B | C deriving (Eq, Ord, Show)
-- >>>
-- >>> let input = Map.fromList [(A, "hello"), (B, "world"), (C, "!")]
-- >>> let (indexMap, arr) = indirect input
-- >>>
-- >>> let f k = maybe undefined (indexArray arr) (Map.lookup k indexMap)
-- >>> (f A, f B, f C)
-- ("hello","world","!")

indirect :: Map k v -> (Map k Int, Array v)
indirect input = runST $ do
    let (enumerated, size) = traverse go input `runState` 0
    output <- newArray size (error "impossible")
    traverse_ (uncurry $ writeArray output) enumerated
    (Map.map fst enumerated,) <$> unsafeFreezeArray output
    where
        go x = get >>= \n -> put (n + 1) >> pure (n, x)

keyArrayQuick :: Map k Int -> Array k
keyArrayQuick input = runArray $ do
    keyArr <- newArray (length input) (error "impossible")
    void $ Map.traverseWithKey (flip $ writeArray keyArr) input
    pure keyArr

toArrays :: Map k v -> (Array k, Array v)
toArrays input =
    let
        (indexMap, arr) = indirect input
    in
        (keyArrayQuick indexMap, arr)