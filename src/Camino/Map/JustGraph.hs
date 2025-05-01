module Camino.Map.JustGraph where

import Camino.Map.Justified (JustMap, Key)
import Camino.Map.Justified qualified as JustMap
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set

-- | Problems reported by 'withJustGraph' exceptions.

data JustGraphProblem a
    = MalformedEdges (Set (a, a))
    deriving Show

-- | Execute a continuation on a /justified graph/. Fails with an exception if the input is
--   missing keys for any of its vertices.

withJustGraph   :: (Ord a, Monad m)
                => Map a [a]
                -> (forall ph. JustMap ph a [Key ph a] -> r)
                -> ExceptT (JustGraphProblem a) m r

withJustGraph input cont = JustMap.withJustMap input $ \m -> checkJustGraph m cont

-- | Internal: check whether a 'JustMap' is indeed a valid justified graph. If so, execute the
--   continuation. Throw an exception otherwise.

checkJustGraph  :: (Ord a, Monad m)
                => JustMap ph a [a]
                -> (forall ph'. JustMap ph' a [Key ph' a] -> r)
                -> ExceptT (JustGraphProblem a) m r

checkJustGraph input cont =
    let
        (output, problems) = runWriter $ JustMap.traverseWithKey (traverse . check) input
    in
        if Set.null problems then
            pure (cont $ catMaybes <$> output)
        else
            throwE (MalformedEdges problems)
    where
        check kx y = case y `JustMap.member` input of
            Just ky -> pure (Just ky)
            Nothing -> report kx y *> pure Nothing
        
        report kx y = tell (Set.singleton (JustMap.forgetKey kx, y))
