-- We ignore redundant constraint on `Ord k`.
-- While this constraint is redundant in this module,
-- `Series k a` where `k` is not an instance of `Ord` would be practically useless
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Data.Series.Conversion ( 
    -- * Conversion to/from Maps
    fromStrictMap,
    toStrictMap,
    fromLazyMap,
    toLazyMap,
    -- * Convertion to/from list
    fromList,
    toList,
) where


import qualified Data.Map.Lazy   as ML
import           Data.Map.Strict ( Map )
import qualified Data.Map.Strict as MS
import qualified Data.Set        as Set
import qualified Data.Vector     as Vector

import           Data.Series.Definition ( Series(..) )


-- | Construct a series from a list of key-value pairs. There is no
-- condition on the order of pairs.
fromList :: (Eq k, Ord k) => [(k, a)] -> Series k a
{-# INLINE fromList #-}
fromList = fromStrictMap . MS.fromList


toList :: Series k a -> [(k, a)]
{-# INLINE toList #-}
toList (MkSeries ks vs) = zip (Set.toAscList ks) (Vector.toList vs)


toLazyMap :: (Eq k, Ord k) => Series k a -> Map k a
{-# INLINE toLazyMap #-}
toLazyMap (MkSeries ks vs) = ML.fromDistinctAscList $ zip (Set.toAscList ks) (Vector.toList vs)


fromLazyMap :: (Eq k, Ord k) => ML.Map k a -> Series k a
{-# INLINE fromLazyMap #-}
fromLazyMap mp = let keys = ML.keysSet mp 
                  in MkSeries { index  = keys 
                              , values = Vector.fromListN (Set.size keys) $ ML.elems mp
                              }


toStrictMap :: (Eq k, Ord k) => Series k a -> Map k a
{-# INLINE toStrictMap #-}
toStrictMap (MkSeries ks vs) = MS.fromDistinctAscList $ zip (Set.toAscList ks) (Vector.toList vs)


fromStrictMap :: (Eq k, Ord k) => MS.Map k a -> Series k a
{-# INLINE fromStrictMap #-}
fromStrictMap mp = MkSeries { index  = MS.keysSet mp
                            , values = Vector.fromListN (MS.size mp) $ MS.elems mp
                            }