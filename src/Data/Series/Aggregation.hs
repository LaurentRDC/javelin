module Data.Series.Aggregation ( 
    GroupBy, 
    groupBy,
    aggregateWith,
) where

import           Data.Bifunctor         ( second )

import           Data.Map.Lazy          ( Map )
import qualified Data.Map.Lazy          as Map

import           Data.Series.Conversion ( fromLazyMap )
import           Data.Series.Definition ( Series(..))
import           Data.Series.View       ( select )
import qualified Data.Set               as Set


-- Original implementation from `extra`'s Data.List.Extra
groupOn :: Eq g => (k -> g) -> [k] -> [ (g, [k]) ]
groupOn _ [] = []
groupOn on (x:xs) = (fx, x:yes) : groupOn on no
    where
        fx = on x
        (yes, no) = span (\y -> fx == on y) xs


groupBy :: (Ord k, Ord g) => Series k a -> (k -> g) ->  GroupBy g k a
groupBy xs by
    = MkGroupBy $ fmap (select xs) groups
    where

        groups = Map.fromList $ fmap (second Set.fromList) $ groupOn by $ Set.toAscList $ index xs


newtype GroupBy g k a 
    -- Note that we're using a lazy map so that groups may or may not
    -- be consumed in their entirety. 
    = MkGroupBy { groups :: Map g (Series k a) }
    deriving (Eq, Show)


aggregateWith :: Ord g => GroupBy g k a -> (Series k a -> b) -> Series g b
aggregateWith (MkGroupBy xs) agg 
    = fromLazyMap (fmap agg xs)