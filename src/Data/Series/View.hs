
module Data.Series.View (
    -- * Accessing a single element
    (!),
    at,
    iat,

    -- * Random bulk access
    select,

    -- * Reindexing
    reindex,

    -- * Accessing ranges
    Range,
    from,
    to,
) where

import           Data.Series.Definition ( Series(..) )
import           Data.Set               ( Set )
import qualified Data.Set               as Set
import qualified Data.Vector            as Vector


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


-- | Datatype representing an inclusive range of keys.
-- The two bounds are expected to be sorted.
data Range k = MkRange k k


-- | Find the keys which are in range
keysInRange :: Ord k => Series k a -> Range k -> (k, k)
keysInRange (MkSeries ks _) (MkRange start stop)
    = let (_, afterStart) = Set.spanAntitone (< start) ks
          inRange         = Set.takeWhileAntitone (<= stop) afterStart
       in (Set.findMin inRange, Set.findMax inRange)


-- | Initiate the slicing of a series by key. This function
-- is expected to be used in conjunction with @to@: 
from :: Ord k => Series k a -> Range k -> Series k a
from series range 
    = let (kstart, kstop) = keysInRange series range 
       in slice (series `indexOf` kstart) (1 + indexOf series kstop) series
    where
        indexOf :: Ord k => Series k a -> k ->  Int
        indexOf (MkSeries ks _) k = Set.findIndex k ks

        -- | Yield a subseries based on indices. The end index is not included.
        slice :: Int -- ^ Start index
              -> Int -- ^ End index
              -> Series k a 
              -> Series k a
        slice start stop (MkSeries ks vs) 
            = let stop' = min (length vs) stop
               in MkSeries { index  = Set.take (stop' - start) $ Set.drop start ks
                           , values = Vector.slice start (stop' - start) vs
                           }


-- | Terminate the slicing of a series by key. This function
-- is expected to be used in conjunction with @from@: 
to :: Ord k => k -> k -> Range k
to k1 k2 = MkRange (min k1 k2) (max k1 k2)


-- | Select all keys in @Set k@ in a series. Keys which are not
-- in the series are ignored.
select :: Ord k => Series k a -> Set k -> Series k a
select (MkSeries ks vs) ss 
    = let selectedKeys = ks `Set.intersection` ss
          newValues = pick <$> Vector.fromListN (Set.size selectedKeys) (Set.toAscList selectedKeys)
        in MkSeries selectedKeys newValues
        where
            pick key = vs Vector.! Set.findIndex key ks


-- | Reindex a series with a new index.
-- Contrary to @select@, all keys in @Set k@ will be present in the re-indexed series.
reindex :: Ord k => Series k a -> Set k -> Series k (Maybe a)
reindex xs ss 
    = let existingKeys = index xs `Set.intersection` ss
          newKeys      = ss `Set.difference` existingKeys
       in (Just <$> (xs `select` existingKeys)) <> MkSeries newKeys (Vector.replicate (Set.size newKeys) Nothing)
