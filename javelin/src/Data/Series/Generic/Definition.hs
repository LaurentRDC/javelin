{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Series.Generic.Definition ( 
    Series(..),

    convert,

    -- * Basic interface
    singleton,
    headM, lastM, map, mapWithKey, mapIndex, concatMap, fold, foldM, 
    foldWithKey, foldMWithKey, foldMap, bifoldMap, foldMapWithKey, 
    length, null, take, takeWhile, drop, dropWhile,
    mapWithKeyM, mapWithKeyM_, forWithKeyM, forWithKeyM_,
    traverseWithKey,

    fromIndex,
    -- * Conversion to/from Series
    IsSeries(..),
    -- ** Conversion to/from Maps
    fromStrictMap,
    toStrictMap,
    fromLazyMap,
    toLazyMap,
    -- ** Conversion to/from list
    fromList,
    toList,
    -- *** Unsafe construction
    fromDistinctAscList,
    -- ** Conversion to/from vectors
    fromVector,
    toVector,
    -- *** Unsafe construction
    fromDistinctAscVector,
    -- ** Handling duplicates
    Occurrence, fromListDuplicates, fromVectorDuplicates,

    -- * Displaying 'Series'
    display, displayWith,
    noLongerThan,
    DisplayOptions(..), defaultDisplayOptions
) where

import           Control.DeepSeq        ( NFData(rnf) )
import           Control.Foldl          ( Fold(..), FoldM(..) )
import           Control.Monad.ST       ( runST )
import           Data.Bifoldable        ( Bifoldable )
import qualified Data.Bifoldable        as Bifoldable
import qualified Data.Foldable          as Foldable
import           Data.Foldable.WithIndex ( FoldableWithIndex(..))
import           Data.Function          ( on )
import           Data.Functor.WithIndex ( FunctorWithIndex(imap) )

import           Data.IntMap.Strict     ( IntMap )
import qualified Data.IntMap.Strict     as IntMap
import qualified Data.List              as List
import qualified Data.Map.Lazy          as ML
import           Data.Map.Strict        ( Map )
import qualified Data.Map.Strict        as MS
import           Data.Sequence          ( Seq )
import qualified Data.Sequence          as Seq
import           Data.Semigroup         ( Semigroup(..) )
import           Data.Series.Index      ( Index )
import qualified Data.Series.Index      as Index
import qualified Data.Series.Index.Internal as Index.Internal
import           Data.Set               ( Set )
import qualified Data.Set               as Set
import           Data.Traversable.WithIndex ( TraversableWithIndex(..) )
import qualified Data.Vector            as Boxed
import           Data.Vector.Algorithms.Intro ( sortUniqBy, sortBy )
import           Data.Vector.Generic    ( Vector )
import qualified Data.Vector.Generic    as Vector
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
 
import           Prelude                hiding ( take, takeWhile, drop, dropWhile, map, concatMap, foldMap, sum, length, null )
import qualified Prelude                as P



-- | A @Series v k a@ is a labeled array of type @v@ filled with values of type @a@,
-- indexed by keys of type @k@.
--
-- Like 'Data.Map.Strict.Map', they support efficient:
--
--      * random access by key ( \(O(\log n)\) );
--      * slice by key ( \(O(\log n)\) ).
--
-- Like 'Data.Vector.Vector', they support efficient:
--
--      * random access by index ( \(O(1)\) );
--      * slice by index ( \(O(1)\) );
--      * numerical operations.
--
data Series v k a 
    -- The reason the index is a set of keys is that we *want* keys to be ordered.
    -- This allows for efficient slicing of the underlying values, because
    -- if @k1 < k2@, then the values are also at indices @ix1 < ix2@.
    = MkSeries { index  :: Index k -- ^ The 'Index' of a series, which contains its (unique) keys in ascending order.
               , values :: v a     -- ^ The values of a series, in the order of its (unique) keys.
               }


-- | \(O(n)\) Convert between two types of 'Series'.
convert :: (Vector v1 a, Vector v2 a) => Series v1 k a -> Series v2 k a
{-# INLINABLE convert #-}
convert (MkSeries ix vs) = MkSeries ix $ Vector.convert vs 


-- | \(O(1)\) Create a 'Series' with a single element.
singleton :: Vector v a => k -> a -> Series v k a
{-# INLINABLE singleton #-}
singleton k v = MkSeries (Index.singleton k) $ Vector.singleton v


-- | \(O(n)\) Generate a 'Series' by mapping every element of its index.
fromIndex :: (Vector v a) 
          => (k -> a) -> Index k -> Series v k a
{-# INLINABLE fromIndex #-}
fromIndex f ix = MkSeries ix $ Vector.convert 
                             $ Boxed.map f -- Using boxed vector to prevent a (Vector v k) constraint
                             $ Index.toAscVector ix


-- | The 'IsSeries' typeclass allow for ad-hoc definition
-- of conversion functions, converting to / from 'Series'.
class IsSeries t v k a where
    -- | Construct a 'Series' from some container of key-values pairs. There is no
    -- condition on the order of pairs. Duplicate keys are silently dropped. If you
    -- need to handle duplicate keys, see 'toSeriesDuplicates'.
    toSeries    :: t -> Series v k a

    -- | Construct a 'Series' from some container of key-values pairs, 
    -- potentially with duplicates.
    -- Each duplicate key @k@ is affixed with an occurence number, starting
    -- with 0. 
    --
    -- @since 0.1.4.0
    toSeriesDuplicates :: t -> Series v (k, Occurrence) a

    -- | Construct a container from key-value pairs of a 'Series'. 
    -- The elements are returned in ascending order of keys. 
    fromSeries  :: Series v k a -> t


instance (Ord k, Vector v a) => IsSeries [(k, a)] v k a where
    -- | Construct a series from a list of key-value pairs. There is no
    -- condition on the order of pairs.
    --
    -- >>> let xs = toSeries [('b', 0::Int), ('a', 5), ('d', 1) ]
    -- >>> xs
    -- index | values
    -- ----- | ------
    --   'a' |      5
    --   'b' |      0
    --   'd' |      1
    --
    -- If you need to handle duplicate keys, take a look at `toSeriesDuplicates`.
    toSeries :: [(k, a)] -> Series v k a
    toSeries = toSeries . MS.fromList
    {-# INLINABLE toSeries #-}

    
    -- | Construct a 'Series' from a list of key-values pairs, 
    -- potentially with duplicates.
    -- Each duplicate key @k@ is affixed with an occurence number, starting
    -- with 0. 
    --
    -- >>> let xs = toSeriesDuplicates [('b', 0::Int), ('a', 5), ('d', 1), ('d', -4), ('d', 7) ]
    -- >>> xs
    --   index | values
    --   ----- | ------
    -- ('a',0) |      5
    -- ('b',0) |      0
    -- ('d',0) |      1
    -- ('d',1) |     -4
    -- ('d',2) |      7
    --
    -- @since 0.1.4.0
    toSeriesDuplicates :: [(k, a)] -> Series v (k, Occurrence) a
    toSeriesDuplicates = fromListDuplicates
    {-# INLINABLE toSeriesDuplicates #-}

    -- | Construct a list from key-value pairs. The elements are in order sorted by key:
    --
    -- >>> let xs = Series.toSeries [ ('b', 0::Int), ('a', 5), ('d', 1) ]
    -- >>> xs
    -- index | values
    -- ----- | ------
    --   'a' |      5
    --   'b' |      0
    --   'd' |      1
    -- >>> fromSeries xs
    -- [('a',5),('b',0),('d',1)]
    fromSeries :: Series v k a -> [(k, a)]
    fromSeries (MkSeries ks vs)= zip (Index.toAscList ks) (Vector.toList vs)
    {-# INLINABLE fromSeries #-}


-- | Construct a 'Series' from a list of key-value pairs. There is no
-- condition on the order of pairs. Duplicate keys are silently dropped. If you
-- need to handle duplicate keys, see 'fromListDuplicates'.
fromList :: (Vector v a, Ord k) => [(k, a)] -> Series v k a
{-# INLINABLE fromList #-}
fromList = toSeries


-- | \(O(n)\) Build a 'Series' from a list of pairs, where the first elements of the pairs (the keys)
-- are distinct elements in ascending order. The precondition that the keys be unique and sorted is not checked.
fromDistinctAscList :: (Vector v a) => [(k, a)] -> Series v k a
fromDistinctAscList xs 
    = let (!ks, !vs) = unzip xs 
       in MkSeries (Index.Internal.fromDistinctAscList ks) (Vector.fromListN (List.length vs) vs)


-- | Integer-like, non-negative number that specifies how many occurrences
-- of a key is present in a 'Series'.
--
-- The easiest way to convert from an 'Occurrence' to another integer-like type
-- is the 'fromIntegral' function.
newtype Occurrence = MkOcc Int
    deriving (Eq, Enum, Num, Ord, Integral, Real)
    deriving newtype (Show, U.Unbox) 

-- Occurrence needs to be an 'U.Unbox' type
-- so that 'fromVectorDuplicates' works with unboxed vectors
-- and series.
newtype instance UM.MVector s Occurrence = MV_Occ (UM.MVector s Int)
newtype instance U.Vector Occurrence = V_Occ (U.Vector Int)
deriving instance GM.MVector UM.MVector Occurrence
deriving instance Vector U.Vector Occurrence 


-- | Construct a series from a list of key-value pairs.
-- Contrary to 'fromList', values at duplicate keys are preserved. To keep each
-- key unique, an 'Occurrence' number counts up.
--
-- See 'toSeriesDuplicates' for an overloaded version of this function which supports many more
-- sequence types, without having to convert to a list.
fromListDuplicates :: (Vector v a, Ord k) => [(k, a)] -> Series v (k, Occurrence) a
{-# INLINABLE fromListDuplicates #-}
fromListDuplicates = convert . fromVectorDuplicates . Boxed.fromList


-- | Construct a list from key-value pairs. The elements are in order sorted by key. 
toList :: Vector v a => Series v k a -> [(k, a)]
{-# INLINABLE toList #-}
toList (MkSeries ks vs) = zip (Index.toAscList ks) (Vector.toList vs)


instance (Ord k) => IsSeries (Boxed.Vector (k, a)) Boxed.Vector k a where
    toSeries = fromVector
    {-# INLINABLE toSeries #-}

    -- | @since 0.1.4.0
    toSeriesDuplicates = fromVectorDuplicates
    {-# INLINABLE toSeriesDuplicates #-}

    fromSeries = toVector
    {-# INLINABLE fromSeries #-}


-- | @since 0.1.4.0
instance (Vector v a, Ord k) => IsSeries (Series v k a) v k a where
    toSeries :: Series v k a -> Series v k a
    toSeries = id
    {-# INLINABLE toSeries #-}

    -- | @since 0.1.4.0
    toSeriesDuplicates :: Series v k a -> Series v (k, Occurrence) a
    toSeriesDuplicates = flip mapIndex (,0::Occurrence)
    {-# INLINABLE toSeriesDuplicates #-}

    fromSeries :: Series v k a -> Series v k a
    fromSeries = id
    {-# INLINABLE fromSeries #-}


instance (Ord k, U.Unbox a, U.Unbox k) => IsSeries (U.Vector (k, a)) U.Vector k a where
    toSeries :: U.Vector (k, a) -> Series U.Vector k a
    toSeries = fromVector
    {-# INLINABLE toSeries #-}

    -- | @since 0.1.4.0
    toSeriesDuplicates :: U.Vector (k, a) -> Series U.Vector (k, Occurrence) a
    toSeriesDuplicates = fromVectorDuplicates
    {-# INLINABLE toSeriesDuplicates #-}

    fromSeries :: Series U.Vector k a -> U.Vector (k, a)
    fromSeries = toVector
    {-# INLINABLE fromSeries #-}


-- | Construct a 'Series' from a 'Vector' of key-value pairs. There is no
-- condition on the order of pairs. Duplicate keys are silently dropped. If you
-- need to handle duplicate keys, see 'fromVectorDuplicates'.
--
-- Note that due to differences in sorting,
-- 'Series.fromList' and @'Series.fromVector' . 'Vector.fromList'@
-- may not be equivalent if the input list contains duplicate keys.
fromVector :: (Ord k, Vector v k, Vector v a, Vector v (k, a))
           => v (k, a) -> Series v k a
{-# INLINABLE fromVector #-}
fromVector vec = let (indexVector, valuesVector) = Vector.unzip $ runST $ do
                        mv <- Vector.thaw vec
                        -- Note that we're using this particular flavor of `sortUniqBy`
                        -- because it both sorts AND removes duplicate keys
                        destMV <- sortUniqBy (compare `on` fst) mv
                        v <- Vector.freeze destMV
                        pure (Vector.force v)
              in MkSeries (Index.Internal.fromDistinctAscVector indexVector) valuesVector


-- | \(O(n)\) Build a 'Series' from a vector of pairs, where the first elements of the pairs (the keys)
-- are distinct elements in ascending order. The precondition that the keys be unique and sorted is not checked.
fromDistinctAscVector :: (Vector v k, Vector v a, Vector v (k, a))
                      => v (k, a) -> Series v k a
fromDistinctAscVector xs 
    = let (ks, vs) = Vector.unzip xs 
       in MkSeries (Index.Internal.fromDistinctAscVector ks) vs


-- | Construct a 'Series' from a 'Vector' of key-value pairs, where there may be duplicate keys. 
-- There is no condition on the order of pairs.
--
-- See 'toSeriesDuplicates' for an overloaded version of this function which supports many more
-- sequence types, without having to convert to a vector.
fromVectorDuplicates :: (Ord k, Vector v k, Vector v a, Vector v (k, a), Vector v (k, Occurrence))
                     => v (k, a) -> Series v (k, Occurrence) a
{-# INLINABLE fromVectorDuplicates #-}
fromVectorDuplicates vec 
    = let (indexVector, valuesVector) 
            = Vector.unzip $ runST $ do
                mv <- Vector.thaw vec
                sortBy (compare `on` fst) mv
                v <- Vector.freeze mv
                pure (Vector.force v)
        in MkSeries (Index.Internal.fromDistinctAscVector (occurences indexVector)) valuesVector
    where
        occurences vs 
            | Vector.null vs        = Vector.empty
            | Vector.length vs == 1 = Vector.map (,0) vs
            | otherwise             = Vector.scanl f (Vector.head vs, 0) (Vector.tail vs)
            where
                f (lastKey, lastOcc) newKey 
                    | lastKey == newKey = (newKey, lastOcc + 1)
                    | otherwise         = (newKey, 0)


-- | Construct a 'Vector' of key-value pairs. The elements are in order sorted by key. 
toVector :: (Vector v a, Vector v k, Vector v (k, a)) 
         => Series v k a -> v (k, a)
{-# INLINABLE toVector #-}
toVector (MkSeries ks vs) = Vector.zip (Index.toAscVector ks) vs


instance (Vector v a) => IsSeries (Map k a) v k a where
    toSeries :: Map k a -> Series v k a
    toSeries mp = MkSeries 
                { index  = Index.fromSet $ MS.keysSet mp
                , values = Vector.fromListN (MS.size mp) $ MS.elems mp
                }
    {-# INLINABLE toSeries #-}

    -- | @since 0.1.4.0
    toSeriesDuplicates :: Map k a -> Series v (k, Occurrence) a
    toSeriesDuplicates = toSeries . MS.mapKeysMonotonic (,0::Occurrence)
    {-# INLINABLE toSeriesDuplicates #-}

    fromSeries :: Series v k a -> Map k a
    fromSeries (MkSeries ks vs)
        = MS.fromDistinctAscList $ zip (Index.toAscList ks) (Vector.toList vs)
    {-# INLINABLE fromSeries #-}


toLazyMap :: (Vector v a) => Series v k a -> Map k a
{-# INLINABLE toLazyMap #-}
toLazyMap = fromSeries


-- | Construct a series from a lazy 'Data.Map.Lazy.Map'.
fromLazyMap :: (Vector v a) => ML.Map k a -> Series v k a
{-# INLINABLE fromLazyMap #-}
fromLazyMap = toSeries


-- | Convert a series into a strict 'Data.Map.Strict.Map'.
toStrictMap :: (Vector v a) => Series v k a -> Map k a
{-# INLINABLE toStrictMap #-}
toStrictMap (MkSeries ks vs) = MS.fromDistinctAscList $ zip (Index.toAscList ks) (Vector.toList vs)


-- | Construct a series from a strict 'Data.Map.Strict.Map'.
fromStrictMap :: (Vector v a) => MS.Map k a -> Series v k a
{-# INLINABLE fromStrictMap #-}
fromStrictMap mp = MkSeries { index  = Index.toIndex $ MS.keysSet mp
                            , values = Vector.fromListN (MS.size mp) $ MS.elems mp
                            }


instance (Vector v a) => IsSeries (IntMap a) v Int a where
    toSeries :: IntMap a -> Series v Int a
    toSeries im = MkSeries 
                { index  = Index.toIndex $ IntMap.keysSet im
                , values = Vector.fromListN (IntMap.size im)  $ IntMap.elems im 
                }
    {-# INLINABLE toSeries #-}

    -- | @since 0.1.4.0
    toSeriesDuplicates :: IntMap a -> Series v (Int, Occurrence) a
    toSeriesDuplicates = fromDistinctAscList 
                       . fmap (\(k::Int, v) -> ( (k, 0::Occurrence), v )) 
                       . IntMap.toAscList
    {-# INLINABLE toSeriesDuplicates #-}

    fromSeries :: Series v Int a -> IntMap a
    fromSeries (MkSeries ks vs) 
        = IntMap.fromDistinctAscList $ zip (Index.toAscList ks) (Vector.toList vs)
    {-# INLINABLE fromSeries #-}


instance (Ord k, Vector v a) => IsSeries (Seq (k, a)) v k a where
    toSeries :: Seq (k, a) -> Series v k a
    toSeries = toSeries . Foldable.toList
    {-# INLINABLE toSeries #-}

    -- | @since 0.1.4.0
    toSeriesDuplicates :: Seq (k, a) -> Series v (k, Occurrence) a
    toSeriesDuplicates = fromListDuplicates . Foldable.toList
    {-# INLINABLE toSeriesDuplicates #-}

    fromSeries :: Series v k a -> Seq (k, a)
    fromSeries = Seq.fromList . fromSeries
    {-# INLINABLE fromSeries #-}


instance (Ord k, Vector v a) => IsSeries (Set (k, a)) v k a where
    toSeries :: Set (k, a) -> Series v k a
    toSeries = fromDistinctAscList . Set.toAscList
    {-# INLINABLE toSeries #-}

    -- | @since 0.1.4.0
    toSeriesDuplicates :: Set (k, a) -> Series v (k, Occurrence) a
    toSeriesDuplicates = fromListDuplicates . Set.toList
    {-# INLINABLE toSeriesDuplicates #-}

    fromSeries :: Series v k a -> Set (k, a)
    fromSeries = Set.fromDistinctAscList . toList
    {-# INLINABLE fromSeries #-}


-- | Get the first value of a 'Series'. If the 'Series' is empty,
-- this function returns 'Nothing'.
headM :: Vector v a => Series v k a -> Maybe a
{-# INLINABLE headM #-}
headM (MkSeries _ vs) = Vector.headM vs


-- | Get the last value of a 'Series'. If the 'Series' is empty,
-- this function returns 'Nothing'.
lastM :: Vector v a => Series v k a -> Maybe a
{-# INLINABLE lastM #-}
lastM (MkSeries _ vs) = Vector.lastM vs


-- | \(O(\log n)\) @'take' n xs@ returns at most @n@ elements of the 'Series' @xs@.
take :: Vector v a => Int -> Series v k a -> Series v k a
{-# INLINABLE take #-}
take n (MkSeries ks vs) 
    -- Index.take is O(log n) while Vector.take is O(1)
    = MkSeries (Index.take n ks) (Vector.take n vs)


-- | \(O(\log n)\) @'drop' n xs@ drops at most @n@ elements from the 'Series' @xs@.
drop :: Vector v a => Int -> Series v k a -> Series v k a
{-# INLINABLE drop #-}
drop n (MkSeries ks vs) 
    -- Index.drop is O(log n) while Vector.drop is O(1)
    = MkSeries (Index.drop n ks) (Vector.drop n vs)


-- | \(O(n)\) Returns the longest prefix (possibly empty) of the input 'Series' that satisfy a predicate.
takeWhile :: Vector v a => (a -> Bool) -> Series v k a -> Series v k a
{-# INLINABLE takeWhile #-}
takeWhile f (MkSeries ix vs) = let taken = Vector.takeWhile f vs
                 in MkSeries { index  = Index.take (Vector.length taken) ix
                             , values = taken 
                             }


-- | \(O(n)\) Returns the complement of 'takeWhile'.
dropWhile :: Vector v a => (a -> Bool) -> Series v k a -> Series v k a
{-# INLINABLE dropWhile #-}
dropWhile f (MkSeries ix vs) = let dropped = Vector.dropWhile f vs
                 in MkSeries { index  = Index.drop (Index.size ix - Vector.length dropped) ix
                             , values = dropped
                             }


-- | \(O(n)\) Map every element of a 'Series'.
map :: (Vector v a, Vector v b) 
    => (a -> b) -> Series v k a -> Series v k b
{-# INLINABLE map #-}
map f (MkSeries ix xs) = MkSeries ix $ Vector.map f xs


-- | \(O(n)\) Map every element of a 'Series', possibly using the key as well.
mapWithKey :: (Vector v a, Vector v b) 
           => (k -> a -> b) -> Series v k a -> Series v k b
{-# INLINABLE mapWithKey #-}
mapWithKey f (MkSeries ix xs) 
    -- We're using boxed vectors to map because we don't want any restrictions
    -- on the index type, i.e. we don't want the constraint Vector v k
    = let vs = Boxed.zipWith f (Index.toAscVector ix) (Vector.convert xs)
       in MkSeries ix (Vector.convert vs)


-- | \(O(n \log n)\).
-- Map each key in the index to another value. Note that the resulting series
-- may have less elements, because each key must be unique.
--
-- In case new keys are conflicting, the first element is kept.
mapIndex :: (Vector v a, Ord k, Ord g) => Series v k a -> (k -> g) -> Series v g a
{-# INLINABLE mapIndex #-}
mapIndex (MkSeries index values) f
    -- Note that the order in which items are kept appears to be backwards;
    -- See the examples for Data.Map.Strict.fromListWith
    = let mapping   = MS.fromListWith (\_ x -> x) $ [(f k, k) | k <- Index.toAscList index]
          newvalues = fmap (\k -> values Vector.! Index.Internal.findIndex k index) mapping
       in toSeries newvalues


-- | Map a function over all the elements of a 'Series' and concatenate the result into a single 'Series'.
concatMap :: (Vector v a, Vector v k, Vector v b, Vector v (k, a), Vector v (k, b), Ord k) 
          => (a -> Series v k b) 
          -> Series v k a 
          -> Series v k b
{-# INLINABLE concatMap #-}
concatMap f = fromVector 
            . Vector.concatMap (toVector . f . snd) 
            . toVector


instance (Vector v a, Ord k) => Semigroup (Series v k a) where
    {-# INLINABLE (<>) #-}
    (<>) :: Series v k a -> Series v k a -> Series v k a
    -- Despite all my effort, merging via conversion to Map remains fastest.
    xs <> ys = toSeries $ toStrictMap xs <> toStrictMap ys

    {-# INLINABLE sconcat #-}
    sconcat = toSeries . sconcat . fmap toStrictMap


instance (Vector v a, Ord k) => Monoid (Series v k a) where
    {-# INLINABLE mempty #-}
    mempty :: Series v k a
    mempty = MkSeries mempty Vector.empty

    {-# INLINABLE mappend #-}
    mappend :: Series v k a -> Series v k a -> Series v k a
    mappend = (<>)

    {-# INLINABLE mconcat #-}
    mconcat :: [Series v k a] -> Series v k a
    mconcat = toSeries . mconcat . fmap toStrictMap


instance (Vector v a, Eq k, Eq a) => Eq (Series v k a) where
    {-# INLINABLE (==) #-}
    (==) :: Series v k a -> Series v k a -> Bool
    (MkSeries ks1 vs1) == (MkSeries ks2 vs2) = (ks1 == ks2) && (vs1 `Vector.eq` vs2)


instance (Vector v a, Ord (v a), Ord k, Ord a) => Ord (Series v k a) where
    {-# INLINABLE compare #-}
    compare :: Series v k a -> Series v k a -> Ordering
    compare (MkSeries ks1 vs1) (MkSeries ks2 vs2) = compare (ks1, vs1) (ks2, vs2)


instance (Functor v) => Functor (Series v k) where
    {-# INLINABLE fmap #-}
    fmap :: (a -> b) -> Series v k a -> Series v k b
    fmap f (MkSeries ks vs) = MkSeries ks (fmap f vs)


instance (forall a. Vector v a, Functor v) => FunctorWithIndex k (Series v k) where
    {-# INLINABLE imap #-}
    imap :: (k -> a -> b) -> Series v k a -> Series v k b
    imap = mapWithKey


-- Inlining all methods in 'Foldable'
-- is important in order for folds over a boxed
-- Series to have performance characteristics
-- be as close as possible to boxed vectors 
instance (Foldable v) => Foldable (Series v k) where
    {-# INLINE fold #-}
    fold :: Monoid m => Series v k m -> m
    fold = Foldable.fold . values

    {-# INLINE foldMap #-}
    foldMap :: (Monoid m) => (a -> m) -> Series v k a -> m
    foldMap f = Foldable.foldMap f . values

    {-# INLINE foldMap' #-}
    foldMap' :: (Monoid m) => (a -> m) -> Series v k a -> m
    foldMap' f = Foldable.foldMap' f . values

    {-# INLINE foldr #-}
    foldr :: (a -> b -> b) -> b -> Series v k a -> b
    foldr f i = Foldable.foldr f i . values

    {-# INLINE foldr' #-}
    foldr' :: (a -> b -> b) -> b -> Series v k a -> b
    foldr' f i = Foldable.foldr' f i . values

    {-# INLINE foldl #-}
    foldl :: (b -> a -> b) -> b -> Series v k a -> b
    foldl f i = Foldable.foldl f i . values

    {-# INLINE foldl' #-}
    foldl' :: (b -> a -> b) -> b -> Series v k a -> b
    foldl' f i = Foldable.foldl' f i . values

    {-# INLINE foldr1 #-}
    foldr1 :: (a -> a -> a) -> Series v k a -> a
    foldr1 f = Foldable.foldr1 f . values

    {-# INLINE foldl1 #-}
    foldl1 :: (a -> a -> a) -> Series v k a -> a
    foldl1 f = Foldable.foldl1 f . values

    {-# INLINE toList #-}
    toList :: Series v k a -> [a]
    toList = Foldable.toList . values

    {-# INLINE null #-}
    null :: Series v k a -> Bool
    null = Foldable.null . values

    {-# INLINE length #-}
    length :: Series v k a -> Int
    length = Foldable.length . values

    {-# INLINE elem #-}
    elem :: Eq a => a -> Series v k a -> Bool
    elem e = Foldable.elem e . values

    {-# INLINE maximum #-}
    maximum :: Ord a => Series v k a -> a
    maximum = Foldable.maximum . values

    {-# INLINE minimum #-}
    minimum :: Ord a => Series v k a -> a
    minimum = Foldable.minimum . values

    {-# INLINE sum #-}
    sum :: Num a => Series v k a -> a
    sum = Foldable.sum . values

    {-# INLINE product #-}
    product :: Num a => Series v k a -> a
    product = Foldable.product . values


instance (forall a. Vector v a, Vector v k, Foldable v, Functor v) => FoldableWithIndex k (Series v k) where
    {-# INLINABLE ifoldMap #-}
    ifoldMap :: Monoid m => (k -> a -> m) -> Series v k a -> m
    ifoldMap = foldMapWithKey


instance (Foldable v) => Bifoldable (Series v) where
    {-# INLINABLE bifoldMap #-}
    bifoldMap :: Monoid m => (k -> m) -> (a -> m) -> Series v k a -> m
    bifoldMap fk fv (MkSeries ks vs) = P.foldMap fk ks <> Foldable.foldMap fv vs


instance (Traversable v) => Traversable (Series v k) where
    {-# INLINABLE traverse #-}
    traverse :: Applicative f
             => (a -> f b) -> Series v k a -> f (Series v k b)
    traverse f (MkSeries ix vs) = MkSeries ix <$> traverse f vs


instance (forall a. Vector v a, Functor v, Foldable v, Ord k, Traversable v) => TraversableWithIndex k (Series v k) where
    {-# INLINABLE itraverse #-}
    itraverse :: Applicative f => (k -> a -> f b) -> Series v k a -> f (Series v k b)
    itraverse = traverseWithKey


-- | \(O(n)\) Execute a 'Fold' over a 'Series'.
--
-- See also 'foldM' for monadic folds, and 'foldWithKey' to take keys into
-- account while folding.
fold :: Vector v a 
     => Fold a b  
     -> Series v k a 
     -> b
fold (Fold step init' extract) 
    = extract . Vector.foldl' step init' . values
{-# INLINABLE fold #-}


-- | \(O(n)\) Execute a monadic 'FoldM' over a 'Series'.
--
-- See also 'fold' for pure folds, and 'foldMWithKey' to take keys into
-- account while folding.
foldM :: (Monad m, Vector v a)
      => FoldM m a b  
      -> Series v k a 
      -> m b
foldM (FoldM step init' extract) xs
    = init' >>= \i -> Vector.foldM' step i (values xs) >>= extract
{-# INLINABLE foldM #-}


-- | \(O(n)\) Execute a 'Fold' over a 'Series', where the 'Fold' takes keys into account.
foldWithKey :: (Vector v a, Vector v k, Vector v (k, a)) 
            => Fold (k, a) b  
            -> Series v k a 
            -> b
foldWithKey (Fold step init' extract) 
    = extract . Vector.foldl' step init' . toVector
{-# INLINABLE foldWithKey #-}


-- | \(O(n)\) Execute a monadic 'FoldM' over a 'Series', where the 'FoldM' takes keys into account.
foldMWithKey :: (Monad m, Vector v a, Vector v k, Vector v (k, a)) 
             => FoldM m (k, a) b
             -> Series v k a 
             -> m b
foldMWithKey (FoldM step init' extract) xs
    = init' >>= \i -> Vector.foldM' step i (toVector xs) >>= extract
{-# INLINABLE foldMWithKey #-}


-- | \(O(n)\) Fold over elements in a 'Series'.
foldMap :: (Monoid m, Vector v a) => (a -> m) -> Series v k a -> m
{-# INLINABLE foldMap #-}
foldMap f = Vector.foldMap f . values


-- | \(O(n)\) Fold over pairs of keys and elements in a 'Series'.
-- See also 'bifoldMap'.
foldMapWithKey :: (Monoid m, Vector v a, Vector v k, Vector v (k, a)) => (k -> a -> m) -> Series v k a -> m
{-# INLINABLE foldMapWithKey #-}
foldMapWithKey f = Vector.foldMap (uncurry f) . toVector


-- | \(O(n)\) Fold over keys and elements separately in a 'Series'.
-- See also 'foldMapWithKey'.
bifoldMap :: (Vector v a, Monoid m) => (k -> m) -> (a -> m) -> Series v k a -> m
{-# INLINABLE bifoldMap #-}
bifoldMap fk fv (MkSeries ks vs) = P.foldMap fk ks <> Vector.foldMap fv vs


-- | \(O(1)\) Extract the length of a 'Series'.
length :: Vector v a => Series v k a -> Int
{-# INLINABLE length #-}
length = Vector.length . values


-- | \(O(1)\) Test whether a 'Series' is empty.
null :: Vector v a => Series v k a -> Bool
{-# INLINABLE null #-}
null = Vector.null . values


-- | \(O(n)\) Apply the monadic action to every element of a series and its
-- index, yielding a series of results.
mapWithKeyM :: (Vector v a, Vector v b, Monad m, Ord k) 
            => (k -> a -> m b) -> Series v k a -> m (Series v k b)
{-# INLINABLE mapWithKeyM #-}
mapWithKeyM f xs = let f' (key, val) = (key,) <$> f key val
           in fmap fromList $ traverse f' $ toList xs


-- | \(O(n)\) Apply the monadic action to every element of a series and its
-- index, discarding the results.
mapWithKeyM_ :: (Vector v a, Monad m) 
             => (k -> a -> m b) -> Series v k a -> m ()
{-# INLINABLE mapWithKeyM_ #-}
mapWithKeyM_ f xs = let f' (key, val) = (key,) <$> f key val
           in mapM_ f' $ toList xs


-- | \(O(n)\) Apply the monadic action to all elements of the series and their associated keys, 
-- yielding a series of results.
forWithKeyM :: (Vector v a, Vector v b, Monad m, Ord k) => Series v k a -> (k -> a -> m b) -> m (Series v k b)
{-# INLINABLE forWithKeyM #-}
forWithKeyM = flip mapWithKeyM


-- | \(O(n)\) Apply the monadic action to all elements of the series and their associated keys, 
-- discarding the results.
forWithKeyM_ :: (Vector v a, Monad m) => Series v k a -> (k -> a -> m b) -> m ()
{-# INLINABLE forWithKeyM_ #-}
forWithKeyM_ = flip mapWithKeyM_


-- | \(O(n)\) Traverse a 'Series' with an Applicative action, taking into account both keys and values. 
traverseWithKey :: (Applicative t, Ord k, Traversable v, Vector v a, Vector v b, Vector v k, Vector v (k, a),  Vector v (k, b))
                => (k -> a -> t b) 
                -> Series v k a 
                -> t (Series v k b)
{-# INLINABLE traverseWithKey #-}
traverseWithKey f = fmap fromVector 
                  . traverse (\(k, x) -> (k,) <$> f k x) 
                  . toVector


instance (NFData (v a), NFData k) => NFData (Series v k a) where
    rnf :: Series v k a -> ()
    rnf (MkSeries ks vs) = rnf ks `seq` rnf vs


instance (Vector v a, Ord k, Show k, Show a) => Show (Series v k a) where
    show :: Series v k a -> String
    show = display


-- | Options controlling how to display 'Series' in the 'displayWith' function.
-- Default options are provided by 'defaultDisplayOptions'.
--
-- To help with creating 'DisplayOptions', see 'noLongerThan'.
data DisplayOptions k a
    = DisplayOptions
    { maximumNumberOfRows  :: Int
    -- ^ Maximum number of rows shown. These rows will be distributed evenly
    -- between the start of the 'Series' and the end. 
    , indexHeader          :: String
    -- ^ Header of the index column.
    , valuesHeader         :: String
    -- ^ Header of the values column.
    , keyDisplayFunction   :: k -> String
    -- ^ Function used to display keys from the 'Series'. Use 'noLongerThan'
    -- to control the width of the index column.
    , valueDisplayFunction :: a -> String
    -- ^ Function used to display values from the 'Series'. Use 'noLongerThan'
    -- to control the width of the values column.
    }


-- | Default 'Series' display options.
defaultDisplayOptions :: (Show k, Show a) => DisplayOptions k a
defaultDisplayOptions 
    = DisplayOptions { maximumNumberOfRows  = 6
                     , indexHeader          = "index"
                     , valuesHeader         = "values"
                     , keyDisplayFunction   = show
                     , valueDisplayFunction = show
                     }


-- | This function modifies existing functions to limit the width of its result.
--
-- >>> let limit7 = (show :: Int -> String) `noLongerThan` 7
-- >>> limit7 123456789
-- "123456..."
noLongerThan :: (a -> String) -> Int -> (a -> String)
noLongerThan f len x 
    = let raw = f x
       in if List.length raw <= max 0 len
        then raw
        else List.take (List.length raw - 3) raw <> "..."


-- | Display a 'Series' using default 'DisplayOptions'.
display :: (Vector v a, Show k, Show a) 
        => Series v k a 
        -> String
display = displayWith defaultDisplayOptions


-- | Display a 'Series' using customizable 'DisplayOptions'.
displayWith :: (Vector v a) 
            => DisplayOptions k a
            -> Series v k a 
            -> String
displayWith DisplayOptions{..} xs
    = formatGrid $ if length xs > max 0 maximumNumberOfRows
        then let headlength = max 0 maximumNumberOfRows `div` 2
                 taillength = max 0 maximumNumberOfRows - headlength
              in mconcat [ [ (keyDisplayFunction k, valueDisplayFunction v) | (k, v) <- toList $ take headlength xs]
                         , [ ("...", "...") ]
                         , [ (keyDisplayFunction k, valueDisplayFunction v) | (k, v) <- toList $ drop (length xs - taillength) xs]
                         ] 
        else [ (keyDisplayFunction k, valueDisplayFunction v) | (k, v) <- toList xs ]

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
                rows' = [ (indexHeader, valuesHeader) ] <> [ ("-----", "------")] <> rows
                (indexCol, valuesCol) = unzip rows'
                width col = maximum (P.length <$> col)
                indexWidth = width indexCol
                valuesWidth = width valuesCol

                -- | Pad a string to a minimum of @n@ characters wide.
                pad :: Int -> String -> String 
                pad n s
                    | n <= P.length s = s
                    | otherwise     = replicate (n - P.length s) ' ' <> s
