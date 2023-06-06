module Data.Series.Generic.Zip (
    zipWith, zipWithMatched,
    replace, (|->), (<-|),
    
    -- * Generalized zipping with strategies
    zipWithStrategy,
    ZipStrategy,
    skipStrategy,
    mapStrategy,
    constStrategy,

    -- * Special case of zipping monoids
    zipWithMonoid,
    esum, eproduct,
) where

import qualified Data.Map.Strict                as Map
import           Data.Monoid                    ( Sum(..), Product(..) )
import           Data.Series.Generic.Definition ( Series(MkSeries, index, values) )
import qualified Data.Series.Generic.Definition as G
import           Data.Series.Generic.View       ( select, requireWith )
import           Data.Vector.Generic            ( Vector )
import qualified Data.Vector.Generic            as Vector
import qualified Data.Series.Index              as Index
import           Prelude                        hiding ( zipWith ) 

-- $setup
-- >>> import qualified Data.Series as Series

infix 6 |->, <-|

-- | Apply a function elementwise to two series, matching elements
-- based on their keys. For keys present only in the left or right series, 
-- the value @Nothing@ is returned.
--
-- >>> let xs = Series.fromList [ ("alpha", 0::Int), ("beta", 1), ("gamma", 2) ]
-- >>> let ys = Series.fromList [ ("alpha", 10::Int), ("beta", 11), ("delta", 13) ]
-- >>> zipWith (+) xs ys
--   index |  values
--   ----- |  ------
-- "alpha" | Just 10
--  "beta" | Just 12
-- "delta" | Nothing
-- "gamma" | Nothing
--
-- To only combine elements where keys are in both series, see @zipWithMatched@
zipWith :: (Vector v a, Vector v b, Vector v c, Vector v (Maybe c), Ord k) 
        => (a -> b -> c) -> Series v k a -> Series v k b -> Series v k (Maybe c)
zipWith f left right
    = let matched = zipWithMatched f left right
          matchedKeys   = index matched
          allKeys       = index left `Index.union` index right
          unmatchedKeys = allKeys `Index.difference` matchedKeys
          unmatched     = MkSeries unmatchedKeys (Vector.replicate (Index.size unmatchedKeys) Nothing)
       in G.map Just matched <> unmatched
{-# INLINE zipWith #-}


-- | Apply a function elementwise to two series, matching elements
-- based on their keys. Keys present only in the left or right series are dropped.
--
-- >>> let xs = Series.fromList [ ("alpha", 0::Int), ("beta", 1), ("gamma", 2) ]
-- >>> let ys = Series.fromList [ ("alpha", 10::Int), ("beta", 11), ("delta", 13) ]
-- >>> zipWithMatched (+) xs ys
--   index | values
--   ----- | ------
-- "alpha" |     10
--  "beta" |     12
--
-- To combine elements where keys are in either series, see @zipWith@
zipWithMatched :: (Vector v a, Vector v b, Vector v c, Ord k) 
               => (a -> b -> c) -> Series v k a -> Series v k b -> Series v k c
zipWithMatched f left right
    = let matchedKeys   = index left `Index.intersection` index right

          (MkSeries _ xs) = left  `select` matchedKeys
          (MkSeries _ ys) = right `select` matchedKeys
          -- The following construction relies on the fact that keys are always sorted
          matched         = MkSeries matchedKeys $ Vector.zipWith f xs ys
       in matched
{-# INLINE zipWithMatched #-}


-- | Replace values from the right series with values from the left series at matching keys.
-- Keys in the right series but not in the right series are unaffected.
replace :: (Vector v a, Vector v Int, Ord k) 
        => Series v k a -> Series v k a -> Series v k a
{-# INLINE replace #-}
xs `replace` ys 
    = let keysToReplace = index xs `Index.intersection` index ys
          iixs          = Index.toAscVector $ Index.mapMonotonic (\k -> Index.findIndex k (index ys)) keysToReplace
       in MkSeries (index ys) $ Vector.update_ (values ys) iixs (values (xs `select` keysToReplace))


(|->) :: (Vector v a, Vector v Int, Ord k)
      => Series v k a -> Series v k a -> Series v k a
{-# INLINE (|->) #-}
(|->) = replace


(<-|) :: (Vector v a, Vector v Int, Ord k) 
      => Series v k a -> Series v k a -> Series v k a
{-# INLINE (<-|)  #-}
(<-|) = flip replace


-- | A `ZipStrategy` is a function which is used to decide what to do when a key is missing from one
-- of two `Series` being zipped together with `zipWithStrategy`.
--
-- If a `ZipStrategy` returns @Nothing@, the key is dropped.
-- If a `ZipStrategy` returns @Just v@ for key @k@, then the value @v@ is inserted at key @k@.
--
-- For example, the most basic `ZipStrategy` is to skip over any key which is missing from the other series.
-- Such a strategy can be written as @skip key value = Nothing@ (see `skipStrategy`).
type ZipStrategy k a b = (k -> a -> Maybe b)


-- | This `ZipStrategy` drops keys which are not present in both `Series`.
--
-- >>> let xs = Series.fromList [ ("alpha", 0::Int), ("beta", 1), ("gamma", 2) ]
-- >>> let ys = Series.fromList [ ("alpha", 10::Int), ("beta", 11), ("delta", 13) ]
-- >>> zipWithStrategy (+) skipStrategy skipStrategy xs ys
--   index | values
--   ----- | ------
-- "alpha" |     10
--  "beta" |     12
skipStrategy :: ZipStrategy k a b
skipStrategy _ _ = Nothing
{-# INLINE skipStrategy #-}


-- | This `ZipStrategy` sets the value at keys which are not present in both `Series` 
-- to the some mapping from the value present in one of the series. See the example below.
--
-- >>> let xs = Series.fromList [ ("alpha", 0::Int), ("beta", 1), ("gamma", 2) ]
-- >>> let ys = Series.fromList [ ("alpha", 5::Int), ("beta", 6), ("delta", 7) ]
-- >>> zipWithStrategy (+) (mapStrategy id) (mapStrategy (*10)) xs ys
--   index | values
--   ----- | ------
-- "alpha" |      5
--  "beta" |      7
-- "delta" |     70
-- "gamma" |      2
mapStrategy :: (a -> b) -> ZipStrategy k a b
mapStrategy f _ x = Just (f x)
{-# INLINE mapStrategy #-}


-- | This `ZipStrategy` sets a constant value at keys which are not present in both `Series`.
--
-- >>> let xs = Series.fromList [ ("alpha", 0::Int), ("beta", 1), ("gamma", 2) ]
-- >>> let ys = Series.fromList [ ("alpha", 10::Int), ("beta", 11), ("delta", 13) ]
-- >>> zipWith (+) xs ys
--   index |  values
--   ----- |  ------
-- "alpha" | Just 10
--  "beta" | Just 12
-- "delta" | Nothing
-- "gamma" | Nothing
-- >>> zipWithStrategy (+) (constStrategy (-100)) (constStrategy 200)  xs ys
--   index | values
--   ----- | ------
-- "alpha" |     10
--  "beta" |     12
-- "delta" |    200
-- "gamma" |   -100
constStrategy :: b -> ZipStrategy k a b
constStrategy v = mapStrategy (const v)
{-# INLINE constStrategy #-}


-- | Zip two `Series` with a combining function, applying a `ZipStrategy` when one key is present in one of the `Series` but not both.
--
-- Note that if you want to drop keys missing in either `Series`, it is faster to use @`zipWithMatched` f@ 
-- than using @`zipWithStrategy` f skipStrategy skipStrategy@.
zipWithStrategy :: (Vector v a, Vector v b, Vector v c, Ord k) 
                => (a -> b -> c)     -- ^ Function to combine values when present in both series
                -> ZipStrategy k a c -- ^ Strategy for when the key is in the left series but not the right
                -> ZipStrategy k b c -- ^ Strategy for when the key is in the right series but not the left
                -> Series v k a
                -> Series v k b 
                -> Series v k c
zipWithStrategy f whenLeft whenRight left right 
    = let onlyLeftKeys  = index left  `Index.difference` index right
          onlyRightKeys = index right `Index.difference` index left

          leftZip =  applyStrategy whenLeft  $ left  `select` onlyLeftKeys
          rightZip = applyStrategy whenRight $ right `select` onlyRightKeys
          
        in zipWithMatched f left right <> leftZip <> rightZip
    where
        -- Application of the `ZipStrategy` is done on a `Map` rather than
        -- the `Series` directly to keep the type contraints of `zipWithStrategy` to
        -- a minimum. Recall that unboxed `Series` cannot contain `Maybe a`.  
        applyStrategy strat = G.fromStrictMap 
                            . Map.mapMaybeWithKey strat
                            . G.toStrictMap
{-# INLINE zipWithStrategy #-}


-- | Zip two `Series` with a combining function. The value for keys which are missing from
-- either `Series` is replaced with the appropriate `mempty` value.
--
-- >>> import Data.Monoid ( Sum(..) )
-- >>> let xs = Series.fromList [ ("2023-01-01", Sum (1::Int)), ("2023-01-02", Sum 2) ]
-- >>> let ys = Series.fromList [ ("2023-01-01", Sum (5::Int)), ("2023-01-03", Sum 7) ]
-- >>> zipWith (<>) xs ys
--        index |                  values
--        ----- |                  ------
-- "2023-01-01" | Just (Sum {getSum = 6})
-- "2023-01-02" |                 Nothing
-- "2023-01-03" |                 Nothing
-- >>> zipWithMonoid (<>) xs ys
--        index |           values
--        ----- |           ------
-- "2023-01-01" | Sum {getSum = 6}
-- "2023-01-02" | Sum {getSum = 2}
-- "2023-01-03" | Sum {getSum = 7}
zipWithMonoid :: ( Monoid a, Monoid b
                 , Vector v a, Vector v b, Vector v c
                 , Ord k
                 ) 
              => (a -> b -> c)
              -> Series v k a
              -> Series v k b 
              -> Series v k c
zipWithMonoid f left right 
    = let fullindex = index left `Index.union` index right
          (MkSeries ix ls) = requireWith (const mempty) id left  fullindex
          (MkSeries _ rs)  = requireWith (const mempty) id right fullindex          
        in MkSeries ix $ Vector.zipWith f ls rs
{-# INLINE zipWithMonoid #-}


-- | Elementwise sum of two `Series`. Elements missing in one or the other `Series` is considered 0. 
--
-- >>> let xs = Series.fromList [ ("2023-01-01", (1::Int)), ("2023-01-02", 2) ]
-- >>> let ys = Series.fromList [ ("2023-01-01", (5::Int)), ("2023-01-03", 7) ]
-- >>> xs `esum` ys
--        index | values
--        ----- | ------
-- "2023-01-01" |      6
-- "2023-01-02" |      2
-- "2023-01-03" |      7
esum :: (Ord k, Num a, Vector v a, Vector v (Sum a)) 
     => Series v k a 
     -> Series v k a
     -> Series v k a
esum ls rs = G.map getSum $ zipWithMonoid (<>) (G.map Sum ls) (G.map Sum rs)
{-# INLINE esum #-}


-- | Elementwise product of two `Series`. Elements missing in one or the other `Series` is considered 1. 
--
-- >>> let xs = Series.fromList [ ("2023-01-01", (2::Int)), ("2023-01-02", 3) ]
-- >>> let ys = Series.fromList [ ("2023-01-01", (5::Int)), ("2023-01-03", 7) ]
-- >>> xs `eproduct` ys
--        index | values
--        ----- | ------
-- "2023-01-01" |     10
-- "2023-01-02" |      3
-- "2023-01-03" |      7
eproduct :: (Ord k, Num a, Vector v a, Vector v (Product a)) 
         => Series v k a 
         -> Series v k a
         -> Series v k a
eproduct ls rs = G.map getProduct $ zipWithMonoid (<>) (G.map Product ls) (G.map Product rs)
{-# INLINE eproduct #-}
