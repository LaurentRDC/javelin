module Data.Series.Definition ( 
    Series(..),
) where

import           Control.DeepSeq ( NFData(rnf) )
import           Data.Bifoldable ( Bifoldable(..) )
import           Data.Foldable   ( Foldable(..) )
import           Data.Set        ( Set )
import           Data.Vector     ( Vector )
import qualified Data.Vector     as Vector


-- | A series is a contiguous array of values of type @a@,
-- indexed by keys of type @k@.
data Series k a 
    = MkSeries {
        -- The reason the index is a set of keys is that we *want* keys to be ordered.
        -- This allows for very fast slicing of the underlying values, because
        -- if `k1 < k2`, then the values are also at indices `ix1 < ix2`.
          index  :: !(Set k)
        , values :: !(Vector a)
               }
    deriving (Show)

instance (Eq k, Eq a) => Eq (Series k a) where
    (==) :: Series k a -> Series k a -> Bool
    (MkSeries ks1 vs1) == (MkSeries ks2 vs2) = (ks1 == ks2) && (vs1 == vs2)

instance Functor (Series k) where
    fmap :: (a -> b) -> Series k a -> Series k b
    fmap f (MkSeries ks vs) = MkSeries ks (Vector.map f vs)


-- Inlining all Foldable methods brings performance to parity with `Data.Vector.Vector`.
-- Otherwise, performance is ~2x slower for `sum`, for example.
instance Foldable (Series k) where

    {-# INLINE fold #-}
    fold :: Monoid m => Series k m -> m
    fold = fold . values

    {-# INLINE foldMap #-}
    foldMap :: Monoid m => (a -> m) -> Series k a -> m
    foldMap f = Vector.foldMap f . values

    {-# INLINE foldMap' #-}
    foldMap' :: Monoid m => (a -> m) -> Series k a -> m
    foldMap' f = Vector.foldMap' f . values

    {-# INLINE foldr #-}
    foldr :: (a -> b -> b) -> b -> Series k a -> b
    foldr f s = Vector.foldr f s . values

    {-# INLINE foldr' #-}
    foldr' :: (a -> b -> b) -> b -> Series k a -> b
    foldr' f s = Vector.foldr' f s . values

    {-# INLINE foldl #-}
    foldl :: (b -> a -> b) -> b -> Series k a -> b
    foldl f s = Vector.foldl f s . values

    {-# INLINE foldl' #-}
    foldl' :: (b -> a -> b) -> b -> Series k a -> b
    foldl' f s = Vector.foldl' f s . values

    {-# INLINE foldr1 #-}
    foldr1 :: (a -> a -> a) -> Series k a -> a
    foldr1 f = Vector.foldr1 f . values

    {-# INLINE foldl1 #-}
    foldl1 :: (a -> a -> a) -> Series k a -> a
    foldl1 f = Vector.foldl1 f . values

    {-# INLINE length #-}
    length :: Series k a -> Int
    length = Vector.length . values

    {-# INLINE null #-}
    null :: Series k a -> Bool
    null = Vector.null . values

    {-# INLINE elem #-}
    elem :: Eq a => a -> Series k a -> Bool
    elem a = Vector.elem a . values

    {-# INLINE maximum #-}
    maximum :: Ord a => Series k a -> a
    maximum = Vector.maximum . values

    {-# INLINE minimum #-}
    minimum :: Ord a => Series k a -> a
    minimum = Vector.minimum . values

    {-# INLINE sum #-}
    sum :: Num a => Series k a -> a
    sum = Vector.sum . values

    {-# INLINE product #-}
    product :: Num a => Series k a -> a
    product = Vector.product . values

instance Bifoldable Series where
    bifoldMap :: Monoid m => (k -> m) -> (a -> m) -> Series k a -> m
    bifoldMap fk fv (MkSeries ks vs) = foldMap fk ks <> foldMap fv vs

instance (NFData k, NFData a) => NFData (Series k a) where
    rnf :: Series k a -> ()
    rnf (MkSeries ks vs) = rnf ks `seq` rnf vs