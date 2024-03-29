module Data.Series.Generic.Zip (
    zipWith, zipWithMatched, zipWithKey,
    zipWith3, zipWithMatched3, zipWithKey3,
    replace, (|->), (<-|),
    
    -- * Generalized zipping with strategies
    zipWithStrategy,
    zipWithStrategy3,
    ZipStrategy,
    skipStrategy,
    mapStrategy,
    constStrategy,

    -- * Special case of zipping monoids
    zipWithMonoid,
    esum, eproduct,

    -- * Unzipping
    unzip, unzip3,
) where

import qualified Data.Map.Strict                as Map
import           Data.Monoid                    ( Sum(..), Product(..) )
import           Data.Series.Generic.Definition ( Series(MkSeries, index, values) )
import qualified Data.Series.Generic.Definition as G
import           Data.Series.Generic.View       ( selectSubset, requireWith )
import           Data.Vector.Generic            ( Vector )
import qualified Data.Vector.Generic            as Vector
import qualified Data.Series.Index              as Index
import qualified Data.Series.Index.Internal     as Index.Internal
import           Prelude                        hiding ( zipWith, zipWith3, unzip, unzip3 ) 

-- $setup
-- >>> import qualified Data.Series as Series

infix 6 |->, <-|

-- | Apply a function elementwise to two series, matching elements
-- based on their keys. For keys present only in the left or right series, 
-- the value 'Nothing' is returned.
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
-- To only combine elements where keys are in both series, see 'zipWithMatched'
zipWith :: (Vector v a, Vector v b, Vector v c, Vector v (Maybe c), Ord k) 
        => (a -> b -> c) -> Series v k a -> Series v k b -> Series v k (Maybe c)
zipWith f left right
    = let matched = zipWithMatched f left right
          matchedKeys   = index matched
          allKeys       = index left `Index.union` index right
          unmatchedKeys = allKeys `Index.difference` matchedKeys
          unmatched     = MkSeries unmatchedKeys (Vector.replicate (Index.size unmatchedKeys) Nothing)
       in G.map Just matched <> unmatched
{-# INLINABLE zipWith #-}


-- | Apply a function elementwise to three series, matching elements
-- based on their keys. For keys present only in the left or right series, 
-- the value 'Nothing' is returned.
--
-- >>> let xs = Series.fromList [ ("alpha", 0::Int),  ("beta", 1),   ("gamma", 2) ]
-- >>> let ys = Series.fromList [ ("alpha", 10::Int), ("beta", 11),  ("delta", 13) ]
-- >>> let zs = Series.fromList [ ("alpha", 20::Int), ("delta", 13), ("epsilon", 6) ]
-- >>> zipWith3 (\x y z -> x + y + z) xs ys zs
--     index |  values
--     ----- |  ------
--   "alpha" | Just 30
--    "beta" | Nothing
--   "delta" | Nothing
-- "epsilon" | Nothing
--   "gamma" | Nothing
--
-- To only combine elements where keys are in all series, see 'zipWithMatched3'
zipWith3 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v (Maybe d), Ord k) 
         => (a -> b -> c -> d) 
         -> Series v k a 
         -> Series v k b 
         -> Series v k c 
         -> Series v k (Maybe d)
zipWith3 f left center right
    = let matched       = zipWithMatched3 f left center right
          matchedKeys   = index matched
          allKeys       = index left `Index.union` index center `Index.union` index right
          unmatchedKeys = allKeys `Index.difference` matchedKeys
          unmatched     = MkSeries unmatchedKeys (Vector.replicate (Index.size unmatchedKeys) Nothing)
       in G.map Just matched <> unmatched
{-# INLINABLE zipWith3 #-}



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
-- To combine elements where keys are in either series, see 'zipWith'. To combine
-- three series, see 'zipWithMatched3'.
zipWithMatched :: (Vector v a, Vector v b, Vector v c, Ord k) 
               => (a -> b -> c) -> Series v k a -> Series v k b -> Series v k c
zipWithMatched f left right
    = let matchedKeys   = index left `Index.intersection` index right
          -- Recall that `selectSubset` is a performance optimization
          -- and is generally unsafe to use; however, in this case, we know
          -- that `matchedKeys` are subsets of the index of both series
          (MkSeries _ !xs) = left  `selectSubset` matchedKeys
          (MkSeries _ !ys) = right `selectSubset` matchedKeys
          -- The following construction relies on the fact that keys are always sorted
       in MkSeries matchedKeys $ Vector.zipWith f xs ys
{-# INLINABLE zipWithMatched #-}


-- | Apply a function elementwise to three series, matching elements
-- based on their keys. Keys not present in all three series are dropped.
--
-- >>> let xs = Series.fromList [ ("alpha", 0::Int),  ("beta", 1),   ("gamma", 2) ]
-- >>> let ys = Series.fromList [ ("alpha", 10::Int), ("beta", 11),  ("delta", 13) ]
-- >>> let zs = Series.fromList [ ("alpha", 20::Int), ("delta", 13), ("epsilon", 6) ]
-- >>> zipWithMatched3 (\x y z -> x + y + z) xs ys zs
--   index | values
--   ----- | ------
-- "alpha" |     30
zipWithMatched3 :: (Vector v a, Vector v b, Vector v c, Vector v d, Ord k) 
                => (a -> b -> c -> d) 
                -> Series v k a 
                -> Series v k b 
                -> Series v k c
                -> Series v k d
zipWithMatched3 f left center right
    = let matchedKeys   = index left `Index.intersection` index center `Index.intersection` index right
          -- Recall that `selectSubset` is a performance optimization
          -- and is generally unsafe to use; however, in this case, we know
          -- that `matchedKeys` are subsets of the index of all series
          (MkSeries _ !xs) = left   `selectSubset` matchedKeys
          (MkSeries _ !ys) = center `selectSubset` matchedKeys
          (MkSeries _ !zs) = right  `selectSubset` matchedKeys
          -- The following construction relies on the fact that keys are always sorted
       in MkSeries matchedKeys $ Vector.zipWith3 f xs ys zs
{-# INLINABLE zipWithMatched3 #-}


-- | Apply a function elementwise to two series, matching elements
-- based on their keys. Keys present only in the left or right series are dropped.
-- 
-- >>> let xs = Series.fromList [ ("alpha", 0::Int), ("beta", 1), ("gamma", 2) ]
-- >>> let ys = Series.fromList [ ("alpha", 10::Int), ("beta", 11), ("delta", 13) ]
-- >>> zipWithKey (\k x y -> length k + x + y) xs ys
--   index | values
--   ----- | ------
-- "alpha" |     15
--  "beta" |     16
--
-- To combine elements where keys are in either series, see 'zipWith'
zipWithKey :: (Vector v a, Vector v b, Vector v c, Vector v k, Ord k) 
           => (k -> a -> b -> c) -> Series v k a -> Series v k b -> Series v k c
zipWithKey f left right
    = let matchedKeys   = index left `Index.intersection` index right
          -- Recall that `selectSubset` is a performance optimization
          -- and is generally unsafe to use; however, in this case, we know
          -- that `matchedKeys` are subsets of the index of both series
          (MkSeries _ xs) = left  `selectSubset` matchedKeys
          (MkSeries _ ys) = right `selectSubset` matchedKeys
          ks              = Index.toAscVector matchedKeys
          -- The following construction relies on the fact that keys are always sorted
       in  MkSeries matchedKeys $ Vector.zipWith3 f ks xs ys
{-# INLINABLE zipWithKey #-}


-- | Apply a function elementwise to three series, matching elements
-- based on their keys. Keys not present in all series are dropped.
-- 
-- >>> let xs = Series.fromList [ ("alpha", 0::Int), ("beta", 1), ("gamma", 2) ]
-- >>> let ys = Series.fromList [ ("alpha", 10::Int), ("beta", 11), ("delta", 13) ]
-- >>> let zs = Series.fromList [ ("alpha", 20::Int), ("beta", 7), ("delta", 5) ]
-- >>> zipWithKey3 (\k x y z -> length k + x + y + z) xs ys zs
--   index | values
--   ----- | ------
-- "alpha" |     35
--  "beta" |     23

zipWithKey3 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v k, Ord k) 
            => (k -> a -> b -> c -> d) 
            -> Series v k a 
            -> Series v k b 
            -> Series v k c
            -> Series v k d
zipWithKey3 f left center right
    = let matchedKeys   = index left `Index.intersection` index right
          -- Recall that `selectSubset` is a performance optimization
          -- and is generally unsafe to use; however, in this case, we know
          -- that `matchedKeys` are subsets of the index of all series
          (MkSeries _ xs) = left   `selectSubset` matchedKeys
          (MkSeries _ ys) = center `selectSubset` matchedKeys
          (MkSeries _ zs) = right  `selectSubset` matchedKeys
          ks              = Index.toAscVector matchedKeys
          -- The following construction relies on the fact that keys are always sorted
       in  MkSeries matchedKeys $ Vector.zipWith4 f ks xs ys zs
{-# INLINABLE zipWithKey3 #-}


-- | Replace values from the right series with values from the left series at matching keys.
-- Keys in the right series but not in the right series are unaffected.
--
-- >>> let xs = Series.fromList [("Paris", 1 :: Int), ("London", 2), ("Lisbon", 4)]
-- >>> xs
--    index | values
--    ----- | ------
-- "Lisbon" |      4
-- "London" |      2
--  "Paris" |      1
-- >>> let ys = Series.singleton "Paris" (99::Int)
-- >>> ys `replace` xs
--    index | values
--    ----- | ------
-- "Lisbon" |      4
-- "London" |      2
--  "Paris" |     99
replace :: (Vector v a, Vector v Int, Ord k) 
        => Series v k a -> Series v k a -> Series v k a
{-# INLINABLE replace #-}
xs `replace` ys 
    = let keysToReplace = index xs `Index.intersection` index ys
          iixs          = Index.toAscVector $ Index.Internal.mapMonotonic (\k -> Index.Internal.findIndex k (index ys)) keysToReplace
       in MkSeries (index ys) $ Vector.update_ (values ys) iixs (values (xs `selectSubset` keysToReplace))


-- | Infix version of 'replace'
--
-- >>> let xs = Series.fromList [("Paris", 1 :: Int), ("London", 2), ("Lisbon", 4)]
-- >>> xs
--    index | values
--    ----- | ------
-- "Lisbon" |      4
-- "London" |      2
--  "Paris" |      1
-- >>> let ys = Series.singleton "Paris" (99::Int)
-- >>> xs <-| ys
--    index | values
--    ----- | ------
-- "Lisbon" |      4
-- "London" |      2
--  "Paris" |     99
(|->) :: (Vector v a, Vector v Int, Ord k)
      => Series v k a -> Series v k a -> Series v k a
{-# INLINABLE (|->) #-}
(|->) = replace


-- | Flipped version of '|->'
--
-- >>> let xs = Series.fromList [("Paris", 1 :: Int), ("London", 2), ("Lisbon", 4)]
-- >>> xs
--    index | values
--    ----- | ------
-- "Lisbon" |      4
-- "London" |      2
--  "Paris" |      1
-- >>> let ys = Series.singleton "Paris" (99::Int)
-- >>> ys |-> xs
--    index | values
--    ----- | ------
-- "Lisbon" |      4
-- "London" |      2
--  "Paris" |     99
(<-|) :: (Vector v a, Vector v Int, Ord k) 
      => Series v k a -> Series v k a -> Series v k a
{-# INLINABLE (<-|)  #-}
(<-|) = flip replace


-- | A 'ZipStrategy' is a function which is used to decide what to do when a key is missing from one
-- of two 'Series' being zipped together with 'zipWithStrategy'.
--
-- If a 'ZipStrategy' returns 'Nothing', the key is dropped.
-- If a 'ZipStrategy' returns @'Just' v@ for key @k@, then the value @v@ is inserted at key @k@.
--
-- For example, the most basic 'ZipStrategy' is to skip over any key which is missing from the other series.
-- Such a strategy can be written as @skip key value = 'Nothing'@ (see 'skipStrategy').
type ZipStrategy k a b = (k -> a -> Maybe b)


-- | This 'ZipStrategy' drops keys which are not present in both 'Series'.
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
{-# INLINABLE skipStrategy #-}


-- | This 'ZipStrategy' sets the value at keys which are not present in both 'Series' 
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
{-# INLINABLE mapStrategy #-}


-- | This 'ZipStrategy' sets a constant value at keys which are not present in both 'Series'.
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
{-# INLINABLE constStrategy #-}


-- | Zip two 'Series' with a combining function, applying a 'ZipStrategy' when one key is present in one of the 'Series' but not both.
--
-- Note that if you want to drop keys missing in either 'Series', it is faster to use @'zipWithMatched' f@ 
-- than using @'zipWithStrategy' f skipStrategy skipStrategy@.
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
          -- Recall that `selectSubset` is a performance optimization
          -- and is generally unsafe to use; however, in this case, we know
          -- that `matchedKeys` are subsets of the index of both series
          leftZip =  applyStrategy whenLeft  $ left  `selectSubset` onlyLeftKeys
          rightZip = applyStrategy whenRight $ right `selectSubset` onlyRightKeys
          
        in zipWithMatched f left right <> leftZip <> rightZip
    where
        -- Application of the 'ZipStrategy' is done on a `Map` rather than
        -- the 'Series' directly to keep the type contraints of `zipWithStrategy` to
        -- a minimum. Recall that unboxed 'Series' cannot contain `Maybe a`.  
        applyStrategy strat = G.toSeries 
                            . Map.mapMaybeWithKey strat
                            . G.fromSeries
{-# INLINABLE zipWithStrategy #-}


-- | Zip three 'Series' with a combining function, applying a 'ZipStrategy' when one key is 
-- present in one of the 'Series' but not all of the others.
--
-- Note that if you want to drop keys missing in either 'Series', it is faster to use @'zipWithMatched3' f@ 
-- than using @'zipWithStrategy3' f skipStrategy skipStrategy skipStrategy@.
zipWithStrategy3 :: (Vector v a, Vector v b, Vector v c, Vector v d, Ord k) 
                => (a -> b -> c -> d) -- ^ Function to combine values when present in all series
                -> ZipStrategy k a d  -- ^ Strategy for when the key is in the left series but not in all the others
                -> ZipStrategy k b d  -- ^ Strategy for when the key is in the center series but not in all the others
                -> ZipStrategy k c d  -- ^ Strategy for when the key is in the right series but not in all the others
                -> Series v k a
                -> Series v k b 
                -> Series v k c
                -> Series v k d
zipWithStrategy3 f whenLeft whenCenter whenRight left center right 
    = let onlyLeftKeys  = index left    `Index.difference` (index center `Index.union` index right)
          onlyCenterKeys = index center `Index.difference` (index left   `Index.union` index right)
          onlyRightKeys = index right   `Index.difference` (index center `Index.union` index left)
          -- Recall that `selectSubset` is a performance optimization
          -- and is generally unsafe to use; however, in this case, we know
          -- that `matchedKeys` are subsets of the index of all series
          leftZip =  applyStrategy whenLeft  $ left     `selectSubset` onlyLeftKeys
          centerZip = applyStrategy whenCenter $ center `selectSubset` onlyCenterKeys
          rightZip = applyStrategy whenRight $ right    `selectSubset` onlyRightKeys
          
        in zipWithMatched3 f left center right <> leftZip <> centerZip <> rightZip
    where
        -- Application of the 'ZipStrategy' is done on a `Map` rather than
        -- the 'Series' directly to keep the type contraints of `zipWithStrategy` to
        -- a minimum. Recall that unboxed 'Series' cannot contain `Maybe a`.  
        applyStrategy strat = G.toSeries 
                            . Map.mapMaybeWithKey strat
                            . G.fromSeries
{-# INLINABLE zipWithStrategy3 #-}


-- | Zip two 'Series' with a combining function. The value for keys which are missing from
-- either 'Series' is replaced with the appropriate 'mempty' value.
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
{-# INLINABLE zipWithMonoid #-}


-- | Elementwise sum of two 'Series'. Elements missing in one or the other 'Series' is considered 0. 
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
{-# INLINABLE esum #-}


-- | Elementwise product of two 'Series'. Elements missing in one or the other 'Series' is considered 1. 
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
{-# INLINABLE eproduct #-}


-- | \(O(n)\) Unzip a 'Series' of 2-tuples.
unzip :: (Vector v a, Vector v b, Vector v (a, b)) 
      => Series v k (a, b)
      -> ( Series v k a
         , Series v k b
         )
unzip (MkSeries ix vs) 
    = let (left, right) = Vector.unzip vs
       in (MkSeries ix left, MkSeries ix right)
{-# INLINABLE unzip #-}


-- | \(O(n)\) Unzip a 'Series' of 3-tuples.
unzip3 :: (Vector v a, Vector v b, Vector v c, Vector v (a, b, c)) 
       => Series v k (a, b, c)
       -> ( Series v k a
          , Series v k b
          , Series v k c
          )
unzip3 (MkSeries ix vs) 
    = let (left, center, right) = Vector.unzip3 vs
       in (MkSeries ix left, MkSeries ix center, MkSeries ix right)
{-# INLINABLE unzip3 #-}
