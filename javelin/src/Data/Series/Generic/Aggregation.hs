module Data.Series.Generic.Aggregation ( 
    -- * Grouping
    Grouping,
    groupBy,
    aggregateWith,
    foldWith,

    -- * Windowing
    expanding,
    windowing,

) where

import qualified Data.Map.Strict                as Map
import           Data.Series.Generic.Definition ( Series(..), fromStrictMap, toList )
import qualified Data.Series.Generic.Definition as GSeries
import           Data.Series.Generic.View       ( Range, slice, select )
import           Data.Vector.Generic            ( Vector )
import qualified Data.Vector.Generic            as Vector
import qualified Data.Vector                    as Boxed
import qualified Data.Series.Index              as Index
import qualified Data.Series.Index.Internal     as Index.Internal
import           Prelude                        hiding ( last )

-- $setup
-- >>> import qualified Data.Series as Series
-- >>> import qualified Data.Set as Set

-- | Group values in a 'Series' by some grouping function (@k -> g@).
-- The provided grouping function is guaranteed to operate on a non-empty 'Series'.
--
-- This function is expected to be used in conjunction with @aggregate@:
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
        -> Grouping k g v a   -- ^ Grouped series
{-# INLINE groupBy #-}
groupBy = MkGrouping


-- | Representation of a 'Series' being grouped.
data Grouping k g v a 
    = MkGrouping (Series v k a)  (k -> g)


-- | Aggregate groups resulting from a call to 'groupBy':
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
--
-- If you want to aggregate groups using a binary function, see 'foldWith' which
-- may be much faster.
aggregateWith :: (Ord k, Ord g, Vector v a, Vector v b) 
              => Grouping k g v a 
              -> (Series v k a -> b) 
              -> Series v g b
{-# INLINE aggregateWith #-}
aggregateWith (MkGrouping xs by) f

    = fromStrictMap $ Map.map f $ selectSubset xs <$> groupedKeys
        where
            groupedKeys
                | Index.null (index xs) = mempty
                | otherwise = Map.unionsWith (<>) $ Boxed.map (\k -> Map.singleton (by k) (Index.singleton k)) 
                                                  $ Index.toAscVector (index xs)

            -- | Implementation of `select` where the selection keys are known
            -- to be a subset of the series. This is a performance optimization and 
            -- therefore is not exposed to users.
            {-# INLINE selectSubset #-}
            selectSubset (MkSeries ks vs) ss 
                = MkSeries ss $ Boxed.convert
                            $ Boxed.map (Vector.unsafeIndex vs)
                            $ Boxed.map (`Index.Internal.findIndex` ks) 
                            $ Index.toAscVector ss


-- | Fold over each group in a 'Grouping' using a binary function.
-- While this is not as expressive as 'aggregateWith', users looking for maximum
-- performance should use 'foldWith' as much as possible.
--
-- >>> type Date = (Int, String)
-- >>> month :: (Date -> String) = snd
-- >>> :{ 
--     let xs = Series.fromList [ ((2020, "January") :: Date,  0 :: Int)
--                              , ((2021, "January"), -5)
--                              , ((2020, "June")   , 20)
--                              , ((2021, "June")   , 25) 
--                              ]
--      in xs `groupBy` month `foldWith` min
-- :}
--     index | values
--     ----- | ------
-- "January" |     -5
--    "June" |     20
foldWith :: (Ord g, Vector v a) 
         => Grouping k g v a
         -> (a -> a -> a)
         -> Series v g a
{-# INLINE foldWith #-}
foldWith (MkGrouping xs by) f 
    = fromStrictMap $ Map.unionsWith f [Map.singleton (by k) v | (k, v) <- toList xs]


-- | Expanding window aggregation.
--
-- >>> import qualified Data.Series as Series 
-- >>> :{ 
--     let (xs :: Series.Series Int Int) 
--          = Series.fromList [ (1, 0)
--                            , (2, 1)
--                            , (3, 2)
--                            , (4, 3)
--                            , (5, 4)
--                            , (6, 5)
--                            ]
--     in (xs `expanding` sum) :: Series.Series Int Int 
-- :}
-- index | values
-- ----- | ------
--     1 |      0
--     2 |      1
--     3 |      3
--     4 |      6
--     5 |     10
--     6 |     15
expanding :: (Vector v a, Vector v b) 
          => Series v k a        -- ^ Series vector
          -> (Series v k a -> b) -- ^ Aggregation function
          -> Series v k b        -- ^ Resulting vector
{-# INLINE expanding #-}
expanding vs f = MkSeries (index vs) $ Vector.unfoldrExactN (GSeries.length vs) go 0
    where
        -- Recall that `slice` does NOT include the right index
        go ix = (f $ slice 0 (ix + 1) vs, ix + 1)


-- | General-purpose window aggregation.
--
-- >>> import qualified Data.Series as Series 
-- >>> import           Data.Series ( to )
-- >>> :{ 
--     let (xs :: Series.Series Int Int) 
--          = Series.fromList [ (1, 0)
--                            , (2, 1)
--                            , (3, 2)
--                            , (4, 3)
--                            , (5, 4)
--                            , (6, 5)
--                            ]
--     in windowing (\k -> k `to` (k + 2)) sum xs
-- :}
-- index | values
-- ----- | ------
--     1 |      3
--     2 |      6
--     3 |      9
--     4 |     12
--     5 |      9
--     6 |      5
windowing :: (Ord k, Vector v a, Vector v b)
          => (k -> Range k)
          -> (Series v k a -> b)
          -> Series v k a
          -> Series v k b
{-# INLINE windowing #-}
windowing range agg series 
    = GSeries.mapWithKey (\k _ -> agg $ series `select` range k) series
