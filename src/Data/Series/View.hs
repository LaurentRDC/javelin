{-# LANGUAGE RecordWildCards #-}
module Data.Series.View (
    -- * Accessing a single element
    (!),
    at,
    iat,

    -- * Random bulk access
    select,

    -- * Resizing
    reindex,
    filter,
    dropna,
    mapIndex,

    -- * Accessing ranges
    Range,
    to,
) where

import           Data.Maybe             ( fromJust, isJust )
import qualified Data.Map.Strict        as Map
import           Data.Series.Definition ( Series(..), fromStrictMap )
import           Data.Set               ( Set )
import qualified Data.Set               as Set
import qualified Data.Vector            as Vector

import           Prelude                hiding ( filter )


-- | \(O(1)\). Extract a single value from a series, by index. 
-- An exception is thrown if the index is out-of-bounds.
--
-- A safer alternative is @iat@, which returns @Nothing@ if the index is
-- out-of-bounds.
(!) :: Series k a -> Int -> a
(MkSeries _ vs) ! ix = (Vector.!) vs ix


-- | \(O(\log n)\). Extract a single value from a series, by key.
at :: Ord k => Series k a -> k -> Maybe a
at (MkSeries ks vs) k = do
    ix <- Set.lookupIndex k ks
    pure $ (Vector.!) vs ix 
{-# INLINE at #-}


-- | \(O(1)\). Extract a single value from a series, by index.
iat :: Series k a -> Int -> Maybe a
iat (MkSeries _ vs) =  (Vector.!?) vs
{-# INLINE iat #-}


-- | \(O(n \log n)\).
-- Map each key in the index to another value. Note that the resulting series
-- may have less elements, because each key must be unique.
--
-- In case new keys are conflicting, the first element is kept.
mapIndex :: (Ord k, Ord g) => Series k a -> (k -> g) -> Series g a
{-# INLINE mapIndex #-}
mapIndex MkSeries{..} f
    -- Note that the order in which items are kept appears to be backwards;
    -- See the examples for Data.Map.Strict.fromListWith
    = let mapping   = Map.fromListWith (\_ x -> x) $ [(f k, k) | k <- Set.toAscList index]
          newvalues = fmap (\k -> values Vector.! Set.findIndex k index) mapping
       in fromStrictMap newvalues


-- | Reindex a series with a new index.
-- Contrary to @select@, all keys in @Set k@ will be present in the re-indexed series.
reindex :: Ord k => Series k a -> Set k -> Series k (Maybe a)
{-# INLINE reindex #-}
reindex xs ss 
    = let existingKeys = index xs `Set.intersection` ss
          newKeys      = ss `Set.difference` existingKeys
       in (Just <$> (xs `select` existingKeys)) <> MkSeries newKeys (Vector.replicate (Set.size newKeys) Nothing)


-- | Filter elements. Only elements for which the predicate is @True@ are kept. 
filter :: Ord k => (a -> Bool) -> Series k a -> Series k a
{-# INLINE filter #-}
filter predicate xs@(MkSeries ks vs) 
    = let nothingIndices = Vector.findIndices predicate vs
          keysToDrop = Set.fromList [Set.elemAt ix ks | ix <- Vector.toList nothingIndices]
          keysToKeep = ks `Set.difference` keysToDrop
       in xs `select` keysToKeep


-- | Drop elements which are not available (NA). 
dropna :: Ord k => Series k (Maybe a) -> Series k a
{-# INLINE dropna #-}
dropna = fmap fromJust . filter isJust


-- | Datatype representing an inclusive range of keys.
-- The two bounds are expected to be sorted.
data Range k = MkRange k k


-- | Find the keys which are in range
keysInRange :: Ord k => Series k a -> Range k -> (k, k)
{-# INLINE keysInRange #-}
keysInRange (MkSeries ks _) (MkRange start stop)
    = let (_, afterStart) = Set.spanAntitone (< start) ks
          inRange         = Set.takeWhileAntitone (<= stop) afterStart
       in (Set.findMin inRange, Set.findMax inRange)


-- | Terminate the slicing of a series by key. This function
-- is expected to be used in conjunction with @from@: 
to :: Ord k => k -> k -> Range k
to k1 k2 = MkRange (min k1 k2) (max k1 k2)


class Selection s where
    select :: Ord k => Series k a -> s k -> Series k a


instance Selection Set where
    -- | Select all keys in @Set k@ in a series. Keys which are not
    -- in the series are ignored.
    select :: Ord k => Series k a -> Set k -> Series k a
    {-# INLINE select #-}
    select (MkSeries ks vs) ss 
        = let selectedKeys = ks `Set.intersection` ss
              newValues = pick <$> Vector.fromListN (Set.size selectedKeys) (Set.toAscList selectedKeys)
           in MkSeries selectedKeys newValues
            where
                pick key = vs Vector.! Set.findIndex key ks


instance Selection Range where
    select :: Ord k => Series k a -> Range k -> Series k a
    {-# INLINE select #-}
    select series range 
        = let (kstart, kstop) = keysInRange series range 
              indexOf xs k = Set.findIndex k (index xs)
           in slice (series `indexOf` kstart) (1 + indexOf series kstop) series
        where
            -- | Yield a subseries based on indices. The end index is not included.
            slice :: Int -- ^ Start index
                  -> Int -- ^ End index
                  -> Series k a 
                  -> Series k a
            {-# INLINE slice #-}
            slice start stop (MkSeries ks vs) 
                = let stop' = min (length vs) stop
                in MkSeries { index  = Set.take (stop' - start) $ Set.drop start ks
                            , values = Vector.slice start (stop' - start) vs
                            }
