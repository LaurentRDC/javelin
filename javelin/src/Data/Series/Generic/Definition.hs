{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Series.Generic.Definition ( 
    Series(..),

    convert,

    -- * Basic interface
    singleton,
    headM, lastM, map, mapWithKey, mapIndex, concatMap, fold, foldM, 
    foldWithKey, foldMWithKey, foldMap, bifoldMap, foldMapWithKey, 
    sum, length, null, take, takeWhile, drop, dropWhile,
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
    Occurrence, fromListDuplicates, fromVectorDuplicates
) where

import           Control.DeepSeq        ( NFData(rnf) )
import           Control.Foldl          ( Fold(..), FoldM(..) )
import qualified Control.Foldl          as Fold
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
import           Data.MonoTraversable   ( MonoFoldable, Element, ofoldlUnwrap, ofoldMUnwrap )
import qualified Data.Series.Index      as Index
import           Data.Series.Index.Internal ( Index(..) )
import qualified Data.Series.Index.Internal as Index.Internal
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
{-# INLINE convert #-}
convert (MkSeries ix vs) = MkSeries ix $ Vector.convert vs 


-- | \(O(1)\) Create a 'Series' with a single element.
singleton :: Vector v a => k -> a -> Series v k a
{-# INLINE singleton #-}
singleton k v = MkSeries (Index.singleton k) $ Vector.singleton v


-- | \(O(n)\) Generate a 'Series' by mapping every element of its index.
fromIndex :: (Vector v a) 
          => (k -> a) -> Index k -> Series v k a
{-# INLINE fromIndex #-}
fromIndex f ix = MkSeries ix $ Vector.convert 
                             $ Boxed.map f -- Using boxed vector to prevent a (Vector v k) constraint
                             $ Index.toAscVector ix


-- | The 'IsSeries' typeclass allow for ad-hoc definition
-- of conversion functions, converting to / from 'Series'.
class IsSeries t v k a where
    -- | Construct a 'Series' from some container of key-values pairs. There is no
    -- condition on the order of pairs. Duplicate keys are silently dropped. If you
    -- need to handle duplicate keys, see 'fromListDuplicates' or 'fromVectorDuplicates'.
    toSeries    :: t -> Series v k a

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
    -- If you need to handle duplicate keys, take a look at `fromListDuplicates`.
    toSeries :: [(k, a)] -> Series v k a
    toSeries = toSeries . MS.fromList
    {-# INLINE toSeries #-}

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
    {-# INLINE fromSeries #-}


-- | Construct a 'Series' from a list of key-value pairs. There is no
-- condition on the order of pairs. Duplicate keys are silently dropped. If you
-- need to handle duplicate keys, see 'fromListDuplicates'.
fromList :: (Vector v a, Ord k) => [(k, a)] -> Series v k a
{-# INLINE fromList #-}
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
fromListDuplicates :: (Vector v a, Ord k) => [(k, a)] -> Series v (k, Occurrence) a
{-# INLINE fromListDuplicates #-}
fromListDuplicates = convert . fromVectorDuplicates . Boxed.fromList


-- | Construct a list from key-value pairs. The elements are in order sorted by key. 
toList :: Vector v a => Series v k a -> [(k, a)]
{-# INLINE toList #-}
toList (MkSeries ks vs) = zip (Index.toAscList ks) (Vector.toList vs)


instance (Ord k) => IsSeries (Boxed.Vector (k, a)) Boxed.Vector k a where
    toSeries = fromVector
    {-# INLINE toSeries #-}

    fromSeries = toVector
    {-# INLINE fromSeries #-}


instance (Ord k, U.Unbox a, U.Unbox k) => IsSeries (U.Vector (k, a)) U.Vector k a where
    toSeries :: U.Vector (k, a) -> Series U.Vector k a
    toSeries = fromVector
    {-# INLINE toSeries #-}

    fromSeries :: Series U.Vector k a -> U.Vector (k, a)
    fromSeries = toVector
    {-# INLINE fromSeries #-}


-- | Construct a 'Series' from a 'Vector' of key-value pairs. There is no
-- condition on the order of pairs. Duplicate keys are silently dropped. If you
-- need to handle duplicate keys, see 'fromVectorDuplicates'.
--
-- Note that due to differences in sorting,
-- 'Series.fromList' and @'Series.fromVector' . 'Vector.fromList'@
-- may not be equivalent if the input list contains duplicate keys.
fromVector :: (Ord k, Vector v k, Vector v a, Vector v (k, a))
           => v (k, a) -> Series v k a
{-# INLINE fromVector #-}
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
fromVectorDuplicates :: (Ord k, Vector v k, Vector v a, Vector v (k, a), Vector v (k, Occurrence))
                     => v (k, a) -> Series v (k, Occurrence) a
{-# INLINE fromVectorDuplicates #-}
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
{-# INLINE toVector #-}
toVector (MkSeries ks vs) = Vector.zip (Index.toAscVector ks) vs


instance (Vector v a) => IsSeries (Map k a) v k a where
    toSeries :: Map k a -> Series v k a
    toSeries mp = MkSeries 
                { index  = Index.fromSet $ MS.keysSet mp
                , values = Vector.fromListN (MS.size mp) $ MS.elems mp
                }
    {-# INLINE toSeries #-}

    fromSeries :: Series v k a -> Map k a
    fromSeries (MkSeries ks vs)
        = MS.fromDistinctAscList $ zip (Index.toAscList ks) (Vector.toList vs)
    {-# INLINE fromSeries #-}


toLazyMap :: (Vector v a) => Series v k a -> Map k a
{-# INLINE toLazyMap #-}
toLazyMap = fromSeries


-- | Construct a series from a lazy 'Data.Map.Lazy.Map'.
fromLazyMap :: (Vector v a) => ML.Map k a -> Series v k a
{-# INLINE fromLazyMap #-}
fromLazyMap = toSeries


-- | Convert a series into a strict 'Data.Map.Strict.Map'.
toStrictMap :: (Vector v a) => Series v k a -> Map k a
{-# INLINE toStrictMap #-}
toStrictMap (MkSeries ks vs) = MS.fromDistinctAscList $ zip (Index.toAscList ks) (Vector.toList vs)


-- | Construct a series from a strict 'Data.Map.Strict.Map'.
fromStrictMap :: (Vector v a) => MS.Map k a -> Series v k a
{-# INLINE fromStrictMap #-}
fromStrictMap mp = MkSeries { index  = Index.fromSet $ MS.keysSet mp
                            , values = Vector.fromListN (MS.size mp) $ MS.elems mp
                            }


instance (Vector v a) => IsSeries (IntMap a) v Int a where
    toSeries :: IntMap a -> Series v Int a
    toSeries im = MkSeries 
                { index  = Index.Internal.fromDistinctAscList $ IntMap.keys im
                , values = Vector.fromListN (IntMap.size im)  $ IntMap.elems im 
                }
    {-# INLINE toSeries #-}

    fromSeries :: Series v Int a -> IntMap a
    fromSeries (MkSeries ks vs) 
        = IntMap.fromDistinctAscList $ zip (Index.toAscList ks) (Vector.toList vs)
    {-# INLINE fromSeries #-}

-- | Get the first value of a 'Series'. If the 'Series' is empty,
-- this function returns 'Nothing'.
headM :: Vector v a => Series v k a -> Maybe a
{-# INLINE headM #-}
headM (MkSeries _ vs) = Vector.headM vs


-- | Get the last value of a 'Series'. If the 'Series' is empty,
-- this function returns 'Nothing'.
lastM :: Vector v a => Series v k a -> Maybe a
{-# INLINE lastM #-}
lastM (MkSeries _ vs) = Vector.lastM vs


-- | \(O(\log n)\) @'take' n xs@ returns at most @n@ elements of the 'Series' @xs@.
take :: Vector v a => Int -> Series v k a -> Series v k a
{-# INLINE take #-}
take n (MkSeries ks vs) 
    -- Index.take is O(log n) while Vector.take is O(1)
    = MkSeries (Index.take n ks) (Vector.take n vs)


-- | \(O(\log n)\) @'drop' n xs@ drops at most @n@ elements from the 'Series' @xs@.
drop :: Vector v a => Int -> Series v k a -> Series v k a
{-# INLINE drop #-}
drop n (MkSeries ks vs) 
    -- Index.drop is O(log n) while Vector.drop is O(1)
    = MkSeries (Index.drop n ks) (Vector.drop n vs)


-- | \(O(n)\) Returns the longest prefix (possibly empty) of the input 'Series' that satisfy a predicate.
takeWhile :: Vector v a => (a -> Bool) -> Series v k a -> Series v k a
{-# INLINE takeWhile #-}
takeWhile f (MkSeries ix vs) = let taken = Vector.takeWhile f vs
                 in MkSeries { index  = Index.take (Vector.length taken) ix
                             , values = taken 
                             }


-- | \(O(n)\) Returns the complement of 'takeWhile'.
dropWhile :: Vector v a => (a -> Bool) -> Series v k a -> Series v k a
{-# INLINE dropWhile #-}
dropWhile f (MkSeries ix vs) = let dropped = Vector.dropWhile f vs
                 in MkSeries { index  = Index.drop (Index.size ix - Vector.length dropped) ix
                             , values = dropped
                             }


-- | \(O(n)\) Map every element of a 'Series'.
map :: (Vector v a, Vector v b) 
    => (a -> b) -> Series v k a -> Series v k b
{-# INLINE map #-}
map f (MkSeries ix xs) = MkSeries ix $ Vector.map f xs


-- | \(O(n)\) Map every element of a 'Series', possibly using the key as well.
mapWithKey :: (Vector v a, Vector v b) 
           => (k -> a -> b) -> Series v k a -> Series v k b
{-# INLINE mapWithKey #-}
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
{-# INLINE mapIndex #-}
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
{-# INLINE concatMap #-}
concatMap f = fromVector 
            . Vector.concatMap (toVector . f . snd) 
            . toVector


instance (Vector v a, Ord k) => Semigroup (Series v k a) where
    {-# INLINE (<>) #-}
    (<>) :: Series v k a -> Series v k a -> Series v k a
    (MkSeries ks1 vs1) <> (MkSeries ks2 vs2)
        = let allKeys = indexed SLeft ks1 `Index.union` indexed SRight ks2
              newValues = Vector.convert (Boxed.map pick $ Index.toAscVector allKeys)
            in MkSeries (Index.Internal.mapMonotonic extract allKeys) newValues
        where            
            pick (SLeft  ix _) = Vector.unsafeIndex vs1 ix
            pick (SRight ix _) = Vector.unsafeIndex vs2 ix
            -- TODO: sort by uniq in vector-algorithms
            indexed :: (Int -> b -> Second Int b) -> Index b -> Index (Second Int b)
            indexed f = Index.Internal.fromDistinctAscVector 
                      . Boxed.map (uncurry f) 
                      . Boxed.indexed 
                      . Index.toAscVector

-- This datatype allows to determine from which of two 'Index' data come from,
-- while forcing comparisons only on the second element of a tuple
data Second a b 
    = SLeft  !a !b
    | SRight !a !b

extract :: Second a b -> b
extract (SLeft  _ y) = y
extract (SRight _ y) = y
{-# INLINE extract #-}

instance Eq b => Eq (Second a b) where
    (==) = (==) `on` extract

instance (Ord b) => Ord (Second a b) where
    compare = compare `on` extract
    {-# INLINE compare #-}


instance (Vector v a, Ord k) => Monoid (Series v k a) where
    {-# INLINE mempty #-}
    mempty :: Series v k a
    mempty = MkSeries mempty Vector.empty

    {-# INLINE mappend #-}
    mappend :: Series v k a -> Series v k a -> Series v k a
    mappend = (<>)


instance (Vector v a, Eq k, Eq a) => Eq (Series v k a) where
    {-# INLINE (==) #-}
    (==) :: Series v k a -> Series v k a -> Bool
    (MkSeries ks1 vs1) == (MkSeries ks2 vs2) = (ks1 == ks2) && (vs1 `Vector.eq` vs2)


instance (Vector v a, Ord (v a), Ord k, Ord a) => Ord (Series v k a) where
    {-# INLINE compare #-}
    compare :: Series v k a -> Series v k a -> Ordering
    compare (MkSeries ks1 vs1) (MkSeries ks2 vs2) = compare (ks1, vs1) (ks2, vs2)


instance (Functor v) => Functor (Series v k) where
    {-# INLINE fmap #-}
    fmap :: (a -> b) -> Series v k a -> Series v k b
    fmap f (MkSeries ks vs) = MkSeries ks (fmap f vs)


instance (forall a. Vector v a, Functor v) => FunctorWithIndex k (Series v k) where
    {-# INLINE imap #-}
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
    foldMap' f = Foldable.foldMap f . values

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
    {-# INLINE ifoldMap #-}
    ifoldMap :: Monoid m => (k -> a -> m) -> Series v k a -> m
    ifoldMap = foldMapWithKey


instance (Foldable v) => Bifoldable (Series v) where
    {-# INLINE bifoldMap #-}
    bifoldMap :: Monoid m => (k -> m) -> (a -> m) -> Series v k a -> m
    bifoldMap fk fv (MkSeries ks vs) = P.foldMap fk ks <> Foldable.foldMap fv vs


instance (Traversable v) => Traversable (Series v k) where
    {-# INLINE traverse #-}
    traverse :: Applicative f
             => (a -> f b) -> Series v k a -> f (Series v k b)
    traverse f (MkSeries ix vs) = MkSeries ix <$> traverse f vs


instance (forall a. Vector v a, Functor v, Foldable v, Ord k, Traversable v) => TraversableWithIndex k (Series v k) where
    {-# INLINE itraverse #-}
    itraverse :: Applicative f => (k -> a -> f b) -> Series v k a -> f (Series v k b)
    itraverse = traverseWithKey


-- | \(O(n)\) Execute a 'Fold' over a 'Series'.
--
-- See also 'foldM' for monadic folds, and 'foldWithKey' to take keys into
-- account while folding.
--
-- The scary type signature means that this function will appropriately specialize for both types
-- of 'Series', either boxed or unboxed. Since unboxed 'Data.Series.Unboxed.Series' aren't 'Foldable', 
-- another mechanism must be used: 'MonoFoldable'.
--
-- See "Data.Series" and "Data.Series.Unboxed" for specialized versions of this function
-- with friendlier type signatures.
fold :: (MonoFoldable (v a)) 
     => Fold (Element (v a)) b  
     -> Series v k a 
     -> b
fold f xs = Fold.purely ofoldlUnwrap f (values xs)
{-# INLINE fold #-}


-- | \(O(n)\) Execute a monadic 'FoldM' over a 'Series'.
--
-- See also 'fold' for pure folds, and 'foldMWithKey' to take keys into
-- account while folding.
--
-- The scary type signature means that this function will appropriately specialize for both types
-- of 'Series', either boxed or unboxed. Since unboxed 'Data.Series.Unboxed.Series' aren't 'Foldable', 
-- another mechanism must be used: 'MonoFoldable'.
--
-- See "Data.Series" and "Data.Series.Unboxed" for specialized versions of this function
-- with friendlier type signatures.
foldM :: (Monad m, MonoFoldable (v a)) 
      => FoldM m (Element (v a)) b  
      -> Series v k a 
      -> m b
foldM f xs = Fold.impurely ofoldMUnwrap f (values xs)
{-# INLINE foldM #-}


-- | \(O(n)\) Execute a 'Fold' over a 'Series', where the 'Fold' takes keys into account.
--
-- The scary type signature means that this function will appropriately specialize for both types
-- of 'Series', either boxed or unboxed. Since unboxed 'Data.Series.Unboxed.Series' aren't 'Foldable', 
-- another mechanism must be used: 'MonoFoldable'.
foldWithKey :: (MonoFoldable (v (k, a)), Vector v a, Vector v k, Vector v (k, a)) 
            => Fold (Element (v (k, a))) b  
            -> Series v k a 
            -> b
foldWithKey f xs = Fold.purely ofoldlUnwrap f (toVector xs)
{-# INLINE foldWithKey #-}


-- | \(O(n)\) Execute a monadic 'FoldM' over a 'Series', where the 'FoldM' takes keys into account.
--
-- The scary type signature means that this function will appropriately specialize for both types
-- of 'Series', either boxed or unboxed. Since unboxed 'Data.Series.Unboxed.Series' aren't 'Foldable', 
-- another mechanism must be used: 'MonoFoldable'.
foldMWithKey :: (Monad m, MonoFoldable (v (k, a)), Vector v a, Vector v k, Vector v (k, a)) 
             => FoldM m (Element (v (k, a))) b  
             -> Series v k a 
             -> m b
foldMWithKey f xs = Fold.impurely ofoldMUnwrap f (toVector xs)
{-# INLINE foldMWithKey #-}


-- | \(O(n)\) Fold over elements in a 'Series'.
foldMap :: (Monoid m, Vector v a) => (a -> m) -> Series v k a -> m
{-# INLINE foldMap #-}
foldMap f = Vector.foldMap f . values


-- | \(O(n)\) Fold over pairs of keys and elements in a 'Series'.
-- See also 'bifoldMap'.
foldMapWithKey :: (Monoid m, Vector v a, Vector v k, Vector v (k, a)) => (k -> a -> m) -> Series v k a -> m
{-# INLINE foldMapWithKey #-}
foldMapWithKey f = Vector.foldMap (uncurry f) . toVector


-- | \(O(n)\) Fold over keys and elements separately in a 'Series'.
-- See also 'foldMapWithKey'.
bifoldMap :: (Vector v a, Monoid m) => (k -> m) -> (a -> m) -> Series v k a -> m
{-# INLINE bifoldMap #-}
bifoldMap fk fv (MkSeries ks vs) = P.foldMap fk ks <> Vector.foldMap fv vs


-- | \(O(n)\) Add all elements in a 'Series'.
-- This function exists because unboxed 'Data.Series.Unboxed' are
-- not 'Foldable', in which case you cannot use functions such as 'Data.Foldable.sum'.
sum :: Num a => Vector v a => Series v k a -> a
{-# INLINE sum #-}
sum = Vector.sum . values


-- | /O(1)/ Test whether a 'Series' is empty.
null :: Vector v a => Series v k a -> Bool
{-# INLINE null #-}
null = Vector.null . values


-- | /O(1)/ Extract the length of a 'Series'.
length :: Vector v a => Series v k a -> Int
{-# INLINE length #-}
length = Vector.length . values


instance (NFData (v a), NFData k) => NFData (Series v k a) where
    rnf :: Series v k a -> ()
    rnf (MkSeries ks vs) = rnf ks `seq` rnf vs


instance (Vector v a, Ord k, Show k, Show a) => Show (Series v k a) where
    show :: Series v k a -> String
    show xs 
        = formatGrid $ if length xs > 6
            then mconcat [ [ (show k, show v) | (k, v) <- List.take 3 (fromSeries xs :: [(k, a)])]
                         , [ ("...", "...") ]
                         , [ (show k, show v) | (k, v) <- List.drop (length xs - 3) (fromSeries xs :: [(k, a)])]
                         ] 
            else [ (show k, show v) | (k, v) <- (fromSeries xs :: [(k, a)]) ]

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
                    width col = maximum (P.length <$> col)
                    indexWidth = width indexCol
                    valuesWidth = width valuesCol

                    -- | Pad a string to a minimum of @n@ characters wide.
                    pad :: Int -> String -> String 
                    pad n s
                        | n <= P.length s = s
                        | otherwise     = replicate (n - P.length s) ' ' <> s


-- | /O(n)/ Apply the monadic action to every element of a series and its
-- index, yielding a series of results.
mapWithKeyM :: (Vector v a, Vector v b, Monad m, Ord k) 
      => (k -> a -> m b) -> Series v k a -> m (Series v k b)
{-# INLINE mapWithKeyM #-}
mapWithKeyM f xs = let f' (key, val) = (key,) <$> f key val
           in fmap fromList $ traverse f' $ toList xs


-- | /O(n)/ Apply the monadic action to every element of a series and its
-- index, discarding the results.
mapWithKeyM_ :: (Vector v a, Monad m) 
       => (k -> a -> m b) -> Series v k a -> m ()
{-# INLINE mapWithKeyM_ #-}
mapWithKeyM_ f xs = let f' (key, val) = (key,) <$> f key val
           in mapM_ f' $ toList xs


-- | /O(n)/ Apply the monadic action to all elements of the series and their associated keys, 
-- yielding a series of results.
forWithKeyM :: (Vector v a, Vector v b, Monad m, Ord k) => Series v k a -> (k -> a -> m b) -> m (Series v k b)
{-# INLINE forWithKeyM #-}
forWithKeyM = flip mapWithKeyM


-- | /O(n)/ Apply the monadic action to all elements of the series and their associated keys, 
-- discarding the results.
forWithKeyM_ :: (Vector v a, Monad m) => Series v k a -> (k -> a -> m b) -> m ()
{-# INLINE forWithKeyM_ #-}
forWithKeyM_ = flip mapWithKeyM_


-- | /O(n)/ Traverse a 'Series' with an Applicative action, taking into account both keys and values. 
traverseWithKey :: (Applicative t, Ord k, Traversable v, Vector v a, Vector v b, Vector v k, Vector v (k, a),  Vector v (k, b))
                => (k -> a -> t b) 
                -> Series v k a 
                -> t (Series v k b)
{-# INLINE traverseWithKey #-}
traverseWithKey f = fmap fromVector 
                  . traverse (\(k, x) -> (k,) <$> f k x) 
                  . toVector