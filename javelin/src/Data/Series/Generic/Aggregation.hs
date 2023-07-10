module Data.Series.Generic.Aggregation ( 
    GroupBy, 
    groupBy,
    aggregateWith,
    foldGroupsWith,
) where

import qualified Data.Map.Strict                as Map
import           Data.Series.Generic.Definition ( Series(..), fromStrictMap, toList )
import           Data.Vector.Generic            ( Vector )
import qualified Data.Vector.Generic            as Vector
import qualified Data.Vector                    as Boxed
import           Data.Series.Index              ( Index )
import qualified Data.Series.Index              as Index
import           Prelude                        hiding ( last )

infixl 9 `groupBy`
infixr 0 `aggregateWith`
infixr 0 `foldGroupsWith`

-- $setup
-- >>> import qualified Data.Series as Series
-- >>> import qualified Data.Set as Set

-- | Group values in a 'Series' by some grouping function (@k -> g@).
-- The provided grouping function is guaranteed to operate on a non-empty 'Series'.
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
groupBy :: Series v k a       -- ^ Input series
        -> (k -> g)           -- ^ Grouping function
        -> GroupBy v g k a    -- ^ Grouped series
{-# INLINE groupBy #-}
groupBy = flip MkGroupBy


-- | Data type representing groups of @Series k a@, indexed by keys of type @g@.
-- See the documentation for @groupBy@.
data GroupBy v g k a 
    = MkGroupBy (k -> g) !(Series v k a)


-- | Aggregate each group in a 'GroupBy' using a binary function.
-- While this is not as expressive as 'aggregateWith', users looking for maximum
-- performance should use 'foldGroupsWith' as much as possible.
foldGroupsWith :: (Ord g, Vector v a) 
               => GroupBy v g k a 
               -> (a -> a -> a) 
               -> Series v g a
{-# INLINE foldGroupsWith #-}
foldGroupsWith (MkGroupBy grouping xs) f 
    = fromStrictMap $ Map.unionsWith f [Map.singleton (grouping k) v | (k, v) <- toList xs]


-- | General-purpose aggregation for a 'GroupBy',
--
-- If you can express your aggregation as a binary function @a -> a -> a@ (for example, @(+)@), then 
-- using 'foldGroupsWith' can be an order of magnitude faster. 
aggregateWith :: (Ord k, Ord g, Vector v a, Vector v b) 
              => GroupBy v g k a 
              -> (Series v k a -> b) 
              ->  Series v g b
{-# INLINE aggregateWith #-}
aggregateWith (MkGroupBy by xs) f
    = fromStrictMap $ Map.map f $ selectSubset xs <$> groupedKeys
    where
        groupedKeys
            | Index.null (index xs) = mempty
            | otherwise = Map.unionsWith (<>) $ Boxed.map (\k -> Map.singleton (by k) (Index.singleton k)) 
                                              $ Index.toAscVector (index xs)

        -- | Implementation of `select` where the selection keys are known
        -- to be a subset of the series. This is a performance optimization and 
        -- therefore is not exposed to users.
        selectSubset :: (Vector v a, Ord k) => Series v k a -> Index k -> Series v k a
        {-# INLINE selectSubset #-}
        selectSubset (MkSeries ks vs) ss 
            = MkSeries ss $ Boxed.convert
                          $ Boxed.map (Vector.unsafeIndex vs)
                          $ Boxed.map (`Index.findIndex` ks) 
                          $ Index.toAscVector ss