
module Data.Series.View (
    -- * Accessing a single element
    (!),
    at,
    iat,

    -- * Accessing ranges
    Range,
    from,
    to,
) where

import qualified Data.Map.Strict        as Map
import           Data.Series.Definition ( Series(..) )
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
    ix <- Map.lookup k ks
    pure $ (Vector.!) vs ix 


-- | \(O(1)\). Extract a single value from a series, by index.
iat :: Series k a -> Int -> Maybe a
iat (MkSeries _ vs) =  (Vector.!?) vs


indexOf :: Ord k => Series k a -> k ->  Int
indexOf (MkSeries ks _) k = ks Map.! k


-- | Yield a subseries based on indices. The end index is not included.
slice :: Int -- ^ Start index
      -> Int -- ^ End index
      -> Series k a 
      -> Series k a
slice start stop (MkSeries ks vs) 
    = let stop' = min (length vs) stop
       in MkSeries { index  = (\v -> v - start) <$> Map.filter (\ix -> ix >= start && ix < stop) ks
                   , values = Vector.slice start (stop' - start) vs
                   }


data Range k = MkRange k k


-- Find the keys which are in range
keysInRange :: Ord k => Series k a -> Range k -> (k, k)
keysInRange (MkSeries ks _) (MkRange start stop)
    = let (_, afterStart) = Map.spanAntitone (< start) ks
          inRange         = Map.takeWhileAntitone (<= stop) afterStart
       in (fst $ Map.findMin inRange, fst $ Map.findMax inRange)


from :: Ord k => Series k a -> Range k -> Series k a
from series range 
    = let (kstart, kstop) = keysInRange series range 
       in slice (series `indexOf` kstart) (1 + indexOf series kstop) series


to :: Ord k => k -> k -> Range k
to k1 k2 = MkRange (min k1 k2) (max k1 k2)


