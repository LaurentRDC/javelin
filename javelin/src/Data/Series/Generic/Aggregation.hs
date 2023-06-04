module Data.Series.Generic.Aggregation ( 
    GroupBy, 
    groupBy,
    aggregateWith,
    -- * Aggregation functions
    first, last
) where

import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict                as Map
import           Data.Series.Generic.Definition ( Series(..), fromStrictMap )
import           Data.Series.Generic.View       ( select )
import           Data.Vector.Generic            ( Vector )
import qualified Data.Vector.Generic            as Vector
import qualified Data.Series.Index              as Index
import           Prelude                        hiding ( last )

infixl 9 `groupBy`
infixr 0 `aggregateWith`

-- $setup
-- >>> import qualified Data.Series as Series
-- >>> import qualified Data.Set as Set

-- | Group values in a @Series@ by some grouping function (@k -> g@).
-- The provided grouping function is guaranteed to operate on a non-empty `Series`.
--
-- This function is expected to be used in conjunction with @aggregateWith@:
-- 
-- >>> type Date = (Int, String)
-- >>> month :: (Date -> String) = snd
-- >>> :{ 
--     let xs = Series.fromList [ ((2020, "January") :: Date,  0 :: Int)
--                              , ((2021, "January"), -5)
--                              , ((2020, "June")   , 20)
--                              , ((2021, "June")   , 25) 
--                              ]
--      in xs `groupBy` month `aggregateWith` minimum
-- :}
--     index | values
--     ----- | ------
-- "January" |     -5
--    "June" |     20
groupBy :: (Vector v a, Ord k, Ord g) 
        => Series v k a       -- ^ Input series
        -> (k -> g)         -- ^ Grouping function
        -> GroupBy v g k a    -- ^ Grouped series
{-# INLINE groupBy #-}
groupBy xs by = MkGroupBy $ select xs <$> groupedKeys
    where
        groupedKeys
            | Index.null (index xs) = mempty
            | otherwise = Map.fromListWith (<>) $ [(by k, Index.singleton k) | k <- Index.toAscList (index xs)]


-- | Data type representing groups of @Series k a@, indexed by keys of type @g@.
-- See the documentation for @groupBy@.
newtype GroupBy v g k a 
    = MkGroupBy { groups :: Map g (Series v k a) }


-- | Aggregate grouped series. This function is expected to be used in conjunction
-- with @groupBy@.
aggregateWith :: (Vector v b) 
              => GroupBy v g k a      -- ^ Grouped series
              -> (Series v k a -> b)  -- ^ Aggregation function
              -> Series v g b         -- ^ Aggregated series
{-# INLINE aggregateWith #-}
aggregateWith gps agg = fromStrictMap $ fmap agg (groups gps) 


-- | Extract the first value out of a `Series`.
first :: Vector v a => Series v k a -> a
{-# INLINE first #-}
first = Vector.head . values


-- | Extract the last value out of a `Series`.
last :: Vector v a => Series v k a -> a
{-# INLINE last #-}
last = Vector.last . values