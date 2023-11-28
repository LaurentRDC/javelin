module Data.Series.Generic.Aggregation ( 
    -- * Grouping
    Grouping,
    groupBy,
    aggregateWith,
    foldWith,

    -- * Windowing
    expanding,
    windowing,

    -- * Folding
    all, any, and, or, sum, product, maximum, maximumOn, minimum, minimumOn,
    argmax, argmin,
) where

import qualified Data.List 
import qualified Data.Map.Strict                as Map
import           Data.Ord                       ( Down(..) )
import           Data.Series.Generic.Definition ( Series(..) )
import qualified Data.Series.Generic.Definition as GSeries
import           Data.Series.Generic.View       ( Range, slice, select )
import qualified Data.Vector                    as Boxed
import           Data.Vector.Generic            ( Vector )
import qualified Data.Vector.Generic            as Vector
import           Prelude                        hiding ( last, null, length, all, any, and, or, sum, product, maximum, minimum )

-- $setup
-- >>> import qualified Data.Series as Series
-- >>> import qualified Data.Set as Set

-- | Group values in a 'Series' by some grouping function (@k -> g@).
-- The provided grouping function is guaranteed to operate on a non-empty 'Series'.
--
-- This function is expected to be used in conjunction with @aggregate@:
-- 
-- >>> import Data.Maybe ( fromMaybe )
-- >>> type Date = (Int, String)
-- >>> month :: (Date -> String) = snd
-- >>> :{ 
--     let xs = Series.fromList [ ((2020, "January") :: Date,  0 :: Int)
--                              , ((2021, "January"), -5)
--                              , ((2020, "June")   , 20)
--                              , ((2021, "June")   , 25) 
--                              ]
--      in xs `groupBy` month `aggregateWith` (fromMaybe 0 . minimum)
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
-- >>> import Data.Maybe ( fromMaybe )
-- >>> type Date = (Int, String)
-- >>> month :: (Date -> String) = snd
-- >>> :{ 
--     let xs = Series.fromList [ ((2020, "January") :: Date,  0 :: Int)
--                              , ((2021, "January"), -5)
--                              , ((2020, "June")   , 20)
--                              , ((2021, "June")   , 25) 
--                              ]
--      in xs `groupBy` month `aggregateWith` (fromMaybe 0 . minimum)
-- :}
--     index | values
--     ----- | ------
-- "January" |     -5
--    "June" |     20
--
-- If you want to aggregate groups using a binary function, see 'foldWith' which
-- may be much faster.
aggregateWith :: (Ord g, Vector v a, Vector v b) 
              => Grouping k g v a 
              -> (Series v k a -> b) 
              -> Series v g b
{-# INLINE aggregateWith #-}
aggregateWith (MkGrouping xs by) f
    = GSeries.fromStrictMap 
    $ fmap (f . GSeries.fromDistinctAscList)
    -- We're using a list fold to limit the number of 
    -- type constraints. This is about as fast as it is 
    -- with a Vector fold
    $ Data.List.foldl' acc mempty 
    $ GSeries.toList xs
    where
        acc !m (key, val) = Map.insertWith (<>) (by key) (Data.List.singleton (key, val)) m


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
    = GSeries.fromStrictMap 
    -- We're using a list fold to limit the number of 
    -- type constraints. This is about as fast as it is 
    -- with a Vector fold
    $ Data.List.foldl' acc mempty 
    $ GSeries.toList xs
    where
        acc !m (key, val) = Map.insertWith f (by key) val m


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


-- | /O(n)/ Check if all elements satisfy the predicate.
all :: Vector v a => (a -> Bool) -> Series v k a -> Bool
{-# INLINE all #-}
all f = Vector.all f . values


-- | /O(n)/ Check if any element satisfies the predicate.
any :: Vector v a => (a -> Bool) -> Series v k a -> Bool
{-# INLINE any #-}
any f = Vector.any f . values


-- | /O(n)/ Check if all elements are 'True'.
and :: Vector v Bool => Series v k Bool -> Bool
{-# INLINE and #-}
and = Vector.and . values


-- | /O(n)/ Check if any element is 'True'.
or :: Vector v Bool => Series v k Bool -> Bool
{-# INLINE or #-}
or = Vector.or . values


-- | /O(n)/ Compute the sum of the elements.
sum :: (Num a, Vector v a) => Series v k a -> a
{-# INLINE sum #-}
sum = Vector.sum . values


-- | /O(n)/ Compute the product of the elements.
product :: (Num a, Vector v a) => Series v k a -> a
{-# INLINE product #-}
product = Vector.product . values


nothingIfEmpty :: Vector v a 
               => (Series v k a -> b) -> (Series v k a -> Maybe b)
nothingIfEmpty f xs = if GSeries.null xs then Nothing else Just (f xs) 


-- | /O(n)/ Yield the maximum element of the series. In case of a tie, the first occurrence wins.
maximum :: (Ord a, Vector v a) => Series v k a -> Maybe a
{-# INLINE maximum #-}
maximum = nothingIfEmpty $ Vector.maximum . values


-- | /O(n)/ @'maximumOn' f xs@ teturns the maximum element of the series @xs@, as determined by the function @f@.
-- In case of a tie, the first occurrence wins.
-- If the 'Series' is empty, @Nothing@ is returned.
maximumOn :: (Ord b, Vector v a) => (a -> b) -> Series v k a -> Maybe a
{-# INLINE maximumOn #-}
maximumOn f = nothingIfEmpty $ Vector.maximumOn f . values


-- | /O(n)/ Yield the minimum element of the series. In case of a tie, the first occurrence wins.
-- If the 'Series' is empty, @Nothing@ is returned.
minimum :: (Ord a, Vector v a) => Series v k a -> Maybe a
{-# INLINE minimum #-}
minimum = nothingIfEmpty $ Vector.minimum . values


-- | /O(n)/ @'minimumOn' f xs@ teturns the minimum element of the series @xs@, as determined by the function @f@.
-- In case of a tie, the first occurrence wins.
-- If the 'Series' is empty, @Nothing@ is returned.
minimumOn :: (Ord b, Vector v a) => (a -> b) -> Series v k a -> Maybe a
{-# INLINE minimumOn #-}
minimumOn f = nothingIfEmpty $ Vector.minimumOn f . values


-- | \(O(n)\) Find the index of the maximum element in the input series.
-- If the input series is empty, 'Nothing' is returned.
--
-- The index of the first occurrence of the maximum element is returned.
--
-- >>> import qualified Data.Series as Series 
-- >>> :{ 
--     let (xs :: Series.Series Int Int) 
--          = Series.fromList [ (1, 0)
--                            , (2, 1)
--                            , (3, 2)
--                            , (4, 7)
--                            , (5, 4)
--                            , (6, 5)
--                            ]
--     in argmax xs 
-- :}
-- Just 4
argmax :: (Ord a, Vector v a)
       => Series v k a
       -> Maybe k
{-# INLINE argmax #-}
argmax xs | GSeries.null xs = Nothing
          | otherwise = Just 
                      . fst 
                      -- We're forcing the use of boxed vectors in order to
                      -- reduce the constraints on the vector instance
                      . Boxed.maximumOn snd 
                      . GSeries.toVector
                      . GSeries.convert
                      $ xs


-- | \(O(n)\) Find the index of the minimum element in the input series.
-- If the input series is empty, 'Nothing' is returned.
--
-- The index of the first occurrence of the minimum element is returned.
--
-- >>> import qualified Data.Series as Series 
-- >>> :{ 
--     let (xs :: Series.Series Int Int) 
--          = Series.fromList [ (1, 1)
--                            , (2, 1)
--                            , (3, 2)
--                            , (4, 0)
--                            , (5, 4)
--                            , (6, 5)
--                            ]
--     in argmin xs 
-- :}
-- Just 4
argmin :: (Ord a, Vector v a, Vector v (Down a))
       => Series v k a
       -> Maybe k
{-# INLINE argmin #-}
argmin = argmax . GSeries.map Down