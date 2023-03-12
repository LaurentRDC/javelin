module Data.Series.Aggregation ( 
    GroupBy, 
    groupBy,
    aggregateWith,
) where

import           Data.Map.Strict        ( Map )
import qualified Data.Map.Strict        as Map
import           Data.Series.Definition ( Series(..), fromStrictMap )
import           Data.Series.View       ( select )
import           Data.Set               ( Set )
import qualified Data.Set               as Set


groupKeys :: (Ord g) => (k -> g) -> Set k -> Map g (Set k)
{-# INLINE groupKeys #-}
groupKeys f ks
    | Set.null ks = mempty
    -- We're accumulating lists because they append very quickly, and THEN convert to Set.
    -- Note that because `ks` is a distinct list of ascending elements, we can
    -- use `Set.fromDistinctDescList`. This is because of the order of (++)
    | otherwise   = fmap Set.fromDistinctDescList 
                  $ Map.fromListWith (++) $ [(f k, [k]) | k <- Set.toAscList ks]


groupBy :: (Ord k, Ord g) 
        => Series k a -> (k -> g) ->  GroupBy g k a
{-# INLINE groupBy #-}
groupBy xs by
    = MkGroupBy $ fmap (select xs) groups
    where
        groups = fromStrictMap $ groupKeys by (index xs)


newtype GroupBy g k a 
    = MkGroupBy { groups :: Series g (Series k a) }
    deriving (Eq, Show)


aggregateWith :: GroupBy g k a -> (Series k a -> b) -> Series g b
{-# INLINE aggregateWith #-}
aggregateWith = flip fmap . groups