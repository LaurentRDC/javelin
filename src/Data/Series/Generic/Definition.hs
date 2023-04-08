-- We ignore redundant constraint on `Ord k`.
-- While this constraint is redundant in this module,
-- `Series v k a` where `k` is not an instance of `Ord` would be practically useless
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Series.Generic.Definition ( 
    Series(..),

    -- * Basic interface
    headM, lastM, take, map, mapWithKey, mapIndex,
    foldMap, bifoldMap, sum, length, null,

    -- * Conversion to/from Maps
    fromStrictMap,
    toStrictMap,
    fromLazyMap,
    toLazyMap,
    -- * Convertion to/from list
    fromList,
    toList,
) where

import           Control.DeepSeq        ( NFData(rnf) )
import qualified Data.Bifoldable        as Bifoldable        
import qualified Data.Foldable          as Foldable
import qualified Data.List              as List
import qualified Data.Map.Lazy          as ML
import           Data.Map.Strict        ( Map )
import qualified Data.Map.Strict        as MS
import           Data.Series.Index      ( Index )
import qualified Data.Series.Index      as Index
import qualified Data.Set               as Set
import qualified Data.Vector            as Boxed
import           Data.Vector.Generic    ( Vector )
import qualified Data.Vector.Generic    as Vector

import           Prelude                hiding ( take, map, foldMap, sum, length, null )
import qualified Prelude                as P
import Data.Bifoldable (Bifoldable)


-- | A @Series v k a@ is a labeled array of type @v@ filled with values of type @a@,
-- indexed by keys of type @k@.
--
-- Like @Data.Map@ and @Data.HashMap@, they support efficient:
--
--      * random access by key ( \(O(\log n)\) );
--      * slice by key ( \(O(\log n)\) ).
--
-- Like @Data.Vector.Vector@, they support efficient:
--
--      * random access by index ( \(O(1)\) );
--      * slice by index ( \(O(1)\) );
--      * numerical operations.
--
data Series v k a 
    -- The reason the index is a set of keys is that we *want* keys to be ordered.
    -- This allows for efficient slicing of the underlying values, because
    -- if `k1 < k2`, then the values are also at indices `ix1 < ix2`.
    = MkSeries { index  :: Index k
               , values :: v a
               }

-- | Construct a series from a list of key-value pairs. There is no
-- condition on the order of pairs.
fromList :: (Vector v a, Eq k, Ord k) => [(k, a)] -> Series v k a
{-# INLINE fromList #-}
fromList = fromStrictMap . MS.fromList


-- | Construct a list from key-value pairs. The elements are in order sorted by key:
toList :: Vector v a => Series v k a -> [(k, a)]
{-# INLINE toList #-}
toList (MkSeries ks vs) = zip (Index.toAscList ks) (Vector.toList vs)


-- | Convert a series into a lazy @Map@.
toLazyMap :: (Vector v a, Eq k, Ord k) => Series v k a -> Map k a
{-# INLINE toLazyMap #-}
toLazyMap (MkSeries ks vs) = ML.fromDistinctAscList $ zip (Index.toAscList ks) (Vector.toList vs)


-- | Construct a series from a lazy @Map@.
fromLazyMap :: (Vector v a, Eq k, Ord k) => ML.Map k a -> Series v k a
{-# INLINE fromLazyMap #-}
fromLazyMap mp = let keys = ML.keysSet mp 
                  in MkSeries { index  = Index.fromSet keys 
                              , values = Vector.fromListN (Set.size keys) $ ML.elems mp
                              }


-- | Convert a series into a strict @Map@.
toStrictMap :: (Vector v a, Eq k, Ord k) => Series v k a -> Map k a
{-# INLINE toStrictMap #-}
toStrictMap (MkSeries ks vs) = MS.fromDistinctAscList $ zip (Index.toAscList ks) (Vector.toList vs)


-- | Construct a series from a strict @Map@.
fromStrictMap :: (Vector v a, Eq k, Ord k) => MS.Map k a -> Series v k a
{-# INLINE fromStrictMap #-}
fromStrictMap mp = MkSeries { index  = Index.fromSet $ MS.keysSet mp
                            , values = Vector.fromListN (MS.size mp) $ MS.elems mp
                            }


headM :: Vector v a => Series v k a -> Maybe a
{-# INLINE headM #-}
headM (MkSeries _ vs) = Vector.headM vs


lastM :: Vector v a => Series v k a -> Maybe a
{-# INLINE lastM #-}
lastM (MkSeries _ vs) = Vector.lastM vs


take :: Vector v a => Int -> Series v k a -> Series v k a
{-# INLINE take #-}
take n (MkSeries ks vs) = MkSeries (Index.take n ks) (Vector.take n vs)


-- | \(O(n)\) Map every element of a `Series`.
map :: (Vector v a, Vector v b) 
    => (a -> b) -> Series v k a -> Series v k b
{-# INLINE map #-}
map f (MkSeries ix xs) = MkSeries ix $ Vector.map f xs


-- | \(O(n)\) Map every element of a `Series`, possibly using the key as well.
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
          newvalues = fmap (\k -> values Vector.! Index.findIndex k index) mapping
       in fromStrictMap newvalues


instance (Vector v a, Ord k) => Semigroup (Series v k a) where
    {-# INLINE (<>) #-}
    (<>) :: Series v k a -> Series v k a -> Series v k a
    (MkSeries ks1 vs1) <> (MkSeries ks2 vs2)
        = let allKeys = ks1 <> ks2
              newValues = Vector.fromListN (Index.size allKeys) (pick <$> Index.toAscList allKeys)
            in MkSeries allKeys newValues
        where
            -- TODO: would it be faster to use Set.disjointUnion 
            --       in order to pick from left and right?
            pick key = case findKeyIndex key of
                Left  ix -> Vector.unsafeIndex vs1 ix
                Right ix -> Vector.unsafeIndex vs2 ix
            
            findKeyIndex k 
                = case k `Index.lookupIndex` ks1 of
                    Just ix -> Left ix
                    -- Not safe but we know `k` is either in `ks1` or `ks2`
                    -- Note that this choice makes (<>) left-biased: if there are duplicate keys,
                    -- the value from the left series is preferred.
                    Nothing -> Right $ k `Index.findIndex` ks2


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


instance (Functor v) => Functor (Series v k) where
    {-# INLINE fmap #-}
    fmap :: (a -> b) -> Series v k a -> Series v k b
    fmap f (MkSeries ks vs) = MkSeries ks (fmap f vs)

instance (Foldable v) => Foldable (Series v k) where
    {-# INLINE foldMap #-}
    foldMap :: (Foldable v, Monoid m) => (a -> m) -> Series v k a -> m
    foldMap f xs = Foldable.foldMap f (values xs)

instance (Foldable v) => Bifoldable (Series v) where
    {-# INLINE bifoldMap #-}
    bifoldMap :: Monoid m => (k -> m) -> (a -> m) -> Series v k a -> m
    bifoldMap fk fv (MkSeries ks vs) = P.foldMap fk ks <> Foldable.foldMap fv vs


foldMap :: (Monoid m, Vector v a) => (a -> m) -> Series v k a -> m
{-# INLINE foldMap #-}
foldMap f = Vector.foldMap f . values


bifoldMap :: (Vector v a, Monoid m) => (k -> m) -> (a -> m) -> Series v k a -> m
bifoldMap fk fv (MkSeries ks vs) = P.foldMap fk ks <> Vector.foldMap fv vs


sum :: Num a => Vector v a => Series v k a -> a
sum = Vector.sum . values


null :: Vector v a => Series v k a -> Bool
null = Vector.null . values


length :: Vector v a => Series v k a -> Int
length = Vector.length . values


instance (NFData (v a), NFData k, NFData a) => NFData (Series v k a) where
    rnf :: Series v k a -> ()
    rnf (MkSeries ks vs) = rnf ks `seq` rnf vs


instance (Vector v a, Show k, Show a) => Show (Series v k a) where
    show :: Series v k a -> String
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
                    width col = maximum (P.length <$> col)
                    indexWidth = width indexCol
                    valuesWidth = width valuesCol

                    -- | Pad a string to a minimum of @n@ characters wide.
                    pad :: Int -> String -> String 
                    pad n s
                        | n <= P.length s = s
                        | otherwise     = replicate (n - P.length s) ' ' <> s