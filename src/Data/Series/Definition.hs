-- We ignore redundant constraint on `Ord k`.
-- While this constraint is redundant in this module,
-- `Series k a` where `k` is not an instance of `Ord` would be practically useless
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Series.Definition ( 
    Series(..),

    -- * Basic interface
    headM, lastM, take,

    -- * Conversion to/from Maps
    fromStrictMap,
    toStrictMap,
    fromLazyMap,
    toLazyMap,
    -- * Convertion to/from list
    fromList,
    toList,
) where

import           Control.DeepSeq ( NFData(rnf) )
import           Data.Bifoldable ( Bifoldable(..) )
import           Data.Foldable   ( Foldable(fold, foldMap', foldr', foldl') )
import qualified Data.List       as List
import qualified Data.Map.Lazy   as ML
import           Data.Map.Strict ( Map )
import qualified Data.Map.Strict as MS
import           Data.Set        ( Set )
import qualified Data.Set        as Set
import           Data.Vector     ( Vector )
import qualified Data.Vector     as Vector

import           Prelude         hiding ( take )

import qualified GHC.Exts        ( IsList(..) )


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
    = MkSeries { index  :: Set k
               , values :: Vector a
               }

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


headM :: Series k a -> Maybe a
{-# INLINE headM #-}
headM (MkSeries _ vs) = Vector.headM vs


lastM :: Series k a -> Maybe a
{-# INLINE lastM #-}
lastM (MkSeries _ vs) = Vector.lastM vs


take :: Int -> Series k a -> Series k a
{-# INLINE take #-}
take n (MkSeries ks vs) = MkSeries (Set.take n ks) (Vector.take n vs)


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
                Left  ix -> Vector.unsafeIndex vs1 ix
                Right ix -> Vector.unsafeIndex vs2 ix
            
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


instance Ord k => GHC.Exts.IsList (Series k a) where
    type Item (Series k a) = (k, a)

    fromList :: Ord k => [GHC.Exts.Item (Series k a)] -> Series k a
    fromList = fromList
    
    toList :: Ord k => Series k a -> [GHC.Exts.Item (Series k a)]
    toList = toList

instance (Show k, Show a) => Show (Series k a) where
    show :: Series k a -> String
    show xs 
        = formatGrid $ if length xs > 6
            then mconcat [ [ (show k, show v) | (k, v) <- List.take 3 $ toList xs]
                         , [ ("...", "...") ]
                         , [ (show k, show v) | (k, v) <- List.drop (length xs - 3) $ toList xs]
                         ] 
            else [ (show k, show v) | (k, v) <- toList xs ]

        where
            -- | Format a grid represented by a list of rows, where every row is a list of items
            -- All columns will have a fixed width
            formatGrid :: [ (String, String) ] -- List of rows
                       -> String
            formatGrid rows = mconcat $ List.intersperse "\n" 
                                      $ [ pad indexWidth k <> " | " <> pad valuesWidth v 
                                        | (k, v) <- rows'
                                        ] 
                where
                    rows' = [ ("index", "values") ] <> [ ("-----", "------")] <> rows
                    (indexCol, valuesCol) = unzip rows'
                    width col = maximum (length <$> col)
                    indexWidth = width indexCol
                    valuesWidth = width valuesCol

                    -- | Pad a string to a minimum of @n@ characters wide.
                    pad :: Int -> String -> String 
                    pad n s
                        | n <= length s = s
                        | otherwise     = replicate (n - length s) ' ' <> s