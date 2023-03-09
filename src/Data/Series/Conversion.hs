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
    -- * Convertion from list
    fromList,
    -- * Conversion from an ordered list of key-value pairs.
    fromAscList,
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
fromList = fromLazyMap . ML.fromList  


-- | Construct a series from a list of key-value pairs, where
-- the keys are in ascending order. The precondition is not checked.
--
-- This function should not be used for safety concerns; it is
-- only used internally in conjunction with @toAscList@ from the
-- containers module, and is exposed for testing.
fromAscList :: (Eq k, Ord k) => [(k, a)] -> Series k a
fromAscList xs 
    = let (keys, values) = unzip xs
       in MkSeries { index  = Set.fromAscList keys
                   , values = Vector.fromList values }


toLazyMap :: (Eq k, Ord k) => Series k a -> Map k a
toLazyMap (MkSeries ks vs) = ML.fromAscList $ zip (Set.toList ks) (Vector.toList vs)


fromLazyMap :: (Eq k, Ord k) => ML.Map k a -> Series k a
fromLazyMap = fromAscList . ML.toAscList


toStrictMap :: (Eq k, Ord k) => Series k a -> Map k a
toStrictMap (MkSeries ks vs) = MS.fromAscList $ zip (Set.toList ks) (Vector.toList vs)


fromStrictMap :: (Eq k, Ord k) => MS.Map k a -> Series k a
fromStrictMap = fromAscList . MS.toAscList