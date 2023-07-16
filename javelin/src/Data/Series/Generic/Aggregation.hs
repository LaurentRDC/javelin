{-# LANGUAGE TypeFamilies    #-}
module Data.Series.Generic.Aggregation ( 
    -- * Grouping
    groupBy,
    foldGroups,

    -- * Windowing
    expanding,
    windowing,
    Windowing(..),
    rollingForwards,
    rollingBackwards,

) where

import qualified Data.Map.Strict                as Map
import           Data.Series.Generic.Definition ( Series(..), fromStrictMap, toList )
import qualified Data.Series.Generic.Definition as GSeries
import           Data.Series.Generic.View       ( Range, slice, select, to )
import           Data.Time.Calendar             ( Day, addDays )
import           Data.Time.Calendar.Quarter     ( Quarter, addQuarters )
import           Data.Time.Clock                ( UTCTime, NominalDiffTime, DiffTime, addUTCTime )
import           Data.Time.Clock.TAI            ( AbsoluteTime, addAbsoluteTime )
import           Data.Time.LocalTime            ( LocalTime, addLocalTime )
import           Data.Vector.Generic            ( Vector )
import qualified Data.Vector.Generic            as Vector
import qualified Data.Vector                    as Boxed
import qualified Data.Series.Index              as Index
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
--      in groupBy month minimum xs
-- :}
--     index | values
--     ----- | ------
-- "January" |     -5
--    "June" |     20
groupBy :: (Ord k, Ord g, Vector v a, Vector v b) 
        => (k -> g)           -- ^ Grouping function
        -> (Series v k a -> b)
        -> Series v k a       -- ^ Input series
        -> Series v g b    -- ^ Grouped series
{-# INLINE groupBy #-}
groupBy by f xs 
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
                            $ Boxed.map (`Index.findIndex` ks) 
                            $ Index.toAscVector ss


-- | Aggregate each group in a 'GroupBy' using a binary function.
-- While this is not as expressive as 'aggregate', users looking for maximum
-- performance should use 'foldGroups' as much as possible.
foldGroups :: (Ord g, Vector v a) 
               => (k -> g)
               -> (a -> a -> a) 
               -> Series v k a
               -> Series v g a
{-# INLINE foldGroups #-}
foldGroups grouping f xs 
    = fromStrictMap $ Map.unionsWith f [Map.singleton (grouping k) v | (k, v) <- toList xs]


-- | Expanding window aggregation.
--
-- >>> import Data.Time.Calendar (Day)
-- >>> import qualified Data.Series as Series 
-- >>> :{ 
--     let (xs :: Series.Series Day Integer) 
--          = Series.fromList [ (read "2023-01-01", 0)
--                            , (read "2023-01-02", 1)
--                            , (read "2023-01-03", 2)
--                            , (read "2023-01-04", 3)
--                            , (read "2023-01-05", 4)
--                            , (read "2023-01-06", 5)
--                            ]
--     in (xs `expanding` sum) :: Series.Series Day Integer 
-- :}
--      index | values
--      ----- | ------
-- 2023-01-01 |      0
-- 2023-01-02 |      1
-- 2023-01-03 |      3
-- 2023-01-04 |      6
-- 2023-01-05 |     10
-- 2023-01-06 |     15
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
-- >>> import Data.Time.Calendar (Day, addDays)
-- >>> import qualified Data.Series as Series 
-- >>> :{ 
--     let (xs :: Series.Series Day Integer) 
--          = Series.fromList [ (read "2023-01-01", 0)
--                            , (read "2023-01-02", 1)
--                            , (read "2023-01-03", 2)
--                            , (read "2023-01-04", 3)
--                            , (read "2023-01-05", 4)
--                            , (read "2023-01-06", 5)
--                            ]
--     in windowing (\k -> k `to` addDays 2 k) sum xs
-- :}
--      index | values
--      ----- | ------
-- 2023-01-01 |      3
-- 2023-01-02 |      6
-- 2023-01-03 |      9
-- 2023-01-04 |     12
-- 2023-01-05 |      9
-- 2023-01-06 |      5
windowing :: (Ord k, Vector v a, Vector v b)
          => (k -> Range k)
          -> (Series v k a -> b)
          -> Series v k a
          -> Series v k b
{-# INLINE windowing #-}
windowing range agg series 
    = GSeries.mapWithKey (\k _ -> agg $ series `select` range k) series


-- | Rolling forwards window aggregation.
--
-- >>> import Data.Time.Calendar (Day)
-- >>> :{ 
--     let (xs :: Series.Series Day Integer) 
--          = Series.fromList [ (read "2023-01-01", 0)
--                            , (read "2023-01-02", 1)
--                            , (read "2023-01-03", 2)
--                            , (read "2023-01-04", 3)
--                            , (read "2023-01-05", 4)
--                            , (read "2023-01-06", 5)
--                            ]
--     in (rollingForwards 2 Series.mean xs) :: Series.Series Day Double 
-- :}
--      index | values
--      ----- | ------
-- 2023-01-01 |    1.0
-- 2023-01-02 |    2.0
-- 2023-01-03 |    3.0
-- 2023-01-04 |    4.0
-- 2023-01-05 |    4.5
-- 2023-01-06 |    5.0
rollingForwards :: (Windowing k, Ord k, Vector v a, Vector v b) 
                => Delta k
                -> (Series v k a -> b)
                -> Series v k a
                -> Series v k b
{-# INLINE rollingForwards #-}
rollingForwards windowSize 
    = windowing (\k -> k `to` (k |+| windowSize))


-- | Rolling backwards window aggregation.
--
-- >>> import Data.Time.Calendar (Day)
-- >>> :{ 
--     let (xs :: Series.Series Day Integer) 
--          = Series.fromList [ (read "2023-01-01", 0)
--                            , (read "2023-01-02", 1)
--                            , (read "2023-01-03", 2)
--                            , (read "2023-01-04", 3)
--                            , (read "2023-01-05", 4)
--                            , (read "2023-01-06", 5)
--                            ]
--     in (rollingBackwards 2 Series.mean xs) :: Series.Series Day Double 
-- :}
--      index | values
--      ----- | ------
-- 2023-01-01 |    0.0
-- 2023-01-02 |    0.5
-- 2023-01-03 |    1.0
-- 2023-01-04 |    2.0
-- 2023-01-05 |    3.0
-- 2023-01-06 |    4.0
rollingBackwards :: (Windowing k, Ord k, Vector v a, Vector v b) 
                 => Delta k
                 -> (Series v k a -> b)
                 -> Series v k a
                 -> Series v k b
{-# INLINE rollingBackwards #-}
rollingBackwards windowSize
    = windowing (\k -> k `to` (k |-| windowSize))


-- | The 'Windowing' class represents a class of keys which can be shifted
-- to define a window.
--
-- This type family may appear strange at first, but consider date-time arithmetic. 
-- The type of a time period (or __window__) is different than the type of a moment in time. 
-- For example, a moment in time can be represented with the type 'Data.Time.Clock.UTCTime', but the 
-- duration between two 'Data.Time.Clock.UTCTime' moments is a 'Data.Time.Clock.NominalDiffTime'.
class Windowing k where
    type Delta k

    -- | Shift a key forwards by some delta
    (|+|) :: k -> Delta k -> k

    -- | Shift a key backwards by dome delta
    (|-|) :: k -> Delta k -> k

instance Windowing Int where
    type (Delta Int) = Int
    (|+|) = (+)
    (|-|) = (-)

instance Windowing Integer where
    type (Delta Integer) = Integer
    (|+|) = (+)
    (|-|) = (-)

instance Windowing Float where
    type (Delta Float) = Float
    (|+|) = (+)
    (|-|) = (-)

instance Windowing Double where
    type (Delta Double) = Double
    (|+|) = (+)
    (|-|) = (-)

instance Windowing UTCTime where
    type (Delta UTCTime) = NominalDiffTime
    (|+|) = flip addUTCTime
    time |-| delta = time |+| (-delta) 

instance Windowing LocalTime where
    type (Delta LocalTime) = NominalDiffTime
    (|+|) = flip addLocalTime
    time |-| delta = time |+| (-delta) 

instance Windowing AbsoluteTime where
    type (Delta AbsoluteTime) = DiffTime
    (|+|) = flip addAbsoluteTime
    time |-| delta = time |+| (-delta) 

instance Windowing Day where
    type (Delta Day) = Integer
    (|+|) = flip addDays
    time |-| delta = time |+| (-delta) 

instance Windowing Quarter where
    type (Delta Quarter) = Integer
    (|+|) = flip addQuarters
    time |-| delta = time |+| (-delta) 