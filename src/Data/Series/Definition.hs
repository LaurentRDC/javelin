module Data.Series.Definition ( 
    Series(..),
) where

import           Control.DeepSeq ( NFData(rnf) )
import           Data.Bifoldable ( Bifoldable(..) )
import           Data.Foldable   ( Foldable(..) )
import           Data.Set        ( Set )
import qualified Data.Set        as Set
import           Data.Vector     ( Vector )
import qualified Data.Vector     as Vector


-- | A series is a labeled array of values of type @a@,
-- indexed by keys of type @k@.
--
-- Like Maps and HashMaps, they support efficient:
--      * random access by key (\(O(\log n)\));
--      * slice by key (\(O(\log n)\)).
--
-- Like Vectors and Arrays, they support efficient:
--      * random access by index (\(O(1)\));
--      * slice by index (\(O(1)\));
--      * numerical operations.
data Series k a 
    -- The reason the index is a set of keys is that we *want* keys to be ordered.
    -- This allows for efficient slicing of the underlying values, because
    -- if `k1 < k2`, then the values are also at indices `ix1 < ix2`.
    = MkSeries { index  :: !(Set k)
               , values :: Vector a
               }
    deriving (Show)


instance Ord k => Semigroup (Series k a) where
    {-# INLINE (<>) #-}
    (<>) :: Series k a -> Series k a -> Series k a
    (MkSeries ks1 vs1) <> (MkSeries ks2 vs2)
        = let allKeys = ks1 `Set.union` ks2
              newValues = pick <$> Vector.fromListN (Set.size allKeys) (Set.toAscList allKeys)
            in MkSeries allKeys newValues
        where
            -- TODO: would it be faster to use Set.disjointUnion 
            --       in order to pick from left and right?
            pick :: k -> a
            pick key = case findKeyIndex key of
                Left  ix -> vs1 Vector.! ix
                Right ix -> vs2 Vector.! ix
            
            findKeyIndex :: k -> Either Int Int
            findKeyIndex k 
                = case k `Set.lookupIndex` ks1 of
                    Just ix -> Left ix
                    -- Not safe but we know `k` is either in `ks1` or `ks2`
                    -- Note that this choice makes (<>) left-biased: if there are duplicate keys,
                    -- the value from the left series is preferred.
                    Nothing -> Right $ k `Set.findIndex` ks2


instance Ord k => Monoid (Series k a) where
    {-# INLINE mempty #-}
    mempty :: Series k a
    mempty = MkSeries mempty mempty

    {-# INLINE mappend #-}
    mappend :: Series k a -> Series k a -> Series k a
    mappend = (<>)


instance (Eq k, Eq a) => Eq (Series k a) where
    {-# INLINE (==) #-}
    (==) :: Series k a -> Series k a -> Bool
    (MkSeries ks1 vs1) == (MkSeries ks2 vs2) = (ks1 == ks2) && (vs1 == vs2)


instance Functor (Series k) where
    {-# INLINE fmap #-}
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