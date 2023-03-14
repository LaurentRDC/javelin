module Data.Series.Aggregation ( 
    GroupBy, 
    groupBy,
    aggregateWith,
) where

import           Data.Map.Strict        ( Map )
import qualified Data.Map.Strict        as Map
import           Data.Series.Definition ( Series(..), fromStrictMap )
import           Data.Series.View       ( select )
import qualified Data.Set               as Set


groupBy :: (Ord k, Ord g) 
        => Series k a -> (k -> g) ->  GroupBy g k a
{-# INLINE groupBy #-}
groupBy xs by = MkGroupBy $ select xs <$> groupedKeys
    where
        groupedKeys
            | Set.null (index xs) = mempty
            | otherwise = Map.fromListWith (<>) $ [(by k, Set.singleton k) | k <- Set.toAscList (index xs)]


newtype GroupBy g k a 
    = MkGroupBy { groups :: Map g (Series k a) }
    deriving (Eq, Show)


aggregateWith :: Ord g => GroupBy g k a -> (Series k a -> b) -> Series g b
{-# INLINE aggregateWith #-}
aggregateWith gps agg = fromStrictMap $ fmap agg (groups gps) 