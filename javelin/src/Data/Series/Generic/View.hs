module Data.Series.Generic.View (
    -- * Accessing a single element
    (!),
    at,
    iat,

    -- * Bulk access
    select,
    slice,
    selectWhere,
    selectSubset,
    Selection,

    -- * Resizing
    require,
    requireWith,
    filter,
    filterWithKey,
    catMaybes,
    dropIndex,

    -- * Creating and accessing ranges
    Range(..),
    to,
    from,
    upto,
) where


import           Data.Functor           ( (<&>) )
import           Data.Maybe             ( fromJust, isJust )
import           Data.Series.Index      ( Index )
import qualified Data.Series.Index      as Index
import qualified Data.Series.Index.Internal as Index.Internal
import           Data.Series.Generic.Definition ( Series(..) )
import qualified Data.Series.Generic.Definition as G
import           Data.Set               ( Set )
import qualified Data.Set               as Set
import qualified Data.Vector            as Boxed
import           Data.Vector.Generic    ( Vector )
import qualified Data.Vector.Generic    as Vector

import           Prelude                hiding ( filter )

-- $setup
-- >>> import qualified Data.Series as Series
-- >>> import qualified Data.Series.Index as Index 

infixr 9 `to` -- Ensure that @to@ binds strongest
infixl 1 `select` 


-- | \(O(1)\). Extract a single value from a series, by index. 
-- An exception is thrown if the index is out-of-bounds.
--
-- A safer alternative is @iat@, which returns 'Nothing' if the index is
-- out-of-bounds.
(!) :: Vector v a => Series v k a -> Int -> a
(MkSeries _ vs) ! ix = (Vector.!) vs ix


-- | \(O(\log n)\). Extract a single value from a series, by key.
at :: (Vector v a, Ord k) => Series v k a -> k -> Maybe a
at (MkSeries ks vs) k = Index.lookupIndex k ks <&> Vector.unsafeIndex vs 
{-# INLINABLE at #-}


-- | \(O(1)\). Extract a single value from a series, by index.
iat :: Vector v a => Series v k a -> Int -> Maybe a
iat (MkSeries _ vs) =  (Vector.!?) vs
{-# INLINABLE iat #-}


-- | Require a series with a new index.
-- Contrary to 'select', all keys in @'Index' k@ will be present in the re-indexed series.
require :: (Vector v a, Vector v (Maybe a), Ord k) 
        => Series v k a -> Index k -> Series v k (Maybe a)
{-# INLINABLE require #-}
require = requireWith (const Nothing) Just


-- | Generalization of 'require', which maps missing keys to values.
-- This is particularly useful for 'Vector' instances which don't support 'Maybe', like "Data.Vector.Unboxed".
requireWith :: (Vector v a, Vector v b, Ord k)
            => (k -> b)  -- ^ Function to apply to keys which are missing from the input series, but required in the input index
            -> (a -> b)  -- ^ Function to apply to values which are in the input series and input index.
            -> Series v k a 
            -> Index k 
            -> Series v k b
{-# INLINABLE requireWith #-}
requireWith replacement f xs ss 
    = let existingKeys = index xs `Index.intersection` ss
          newKeys      = ss `Index.difference` existingKeys
       in G.map f (xs `selectSubset` existingKeys) <> MkSeries newKeys (Vector.fromListN (Index.size newKeys) (replacement <$> Index.toAscList newKeys))


-- | \(O(n)\) Drop the index of a series by replacing it with an @Int@-based index. Values will
-- be indexed from 0.
dropIndex :: Series v k a -> Series v Int a
{-# INLINABLE dropIndex #-}
dropIndex (MkSeries ks vs) = MkSeries (Index.Internal.fromDistinctAscList [0..Index.size ks - 1]) vs


-- | Filter elements. Only elements for which the predicate is @True@ are kept. 
-- Notice that the filtering is done on the values, not on the keys; see 'filterWithKey'
-- to filter while taking keys into account.
filter :: (Vector v a, Vector v Int, Ord k) 
       => (a -> Bool) -> Series v k a -> Series v k a
{-# INLINABLE filter #-}
filter predicate xs@(MkSeries ks vs) 
    = let indicesToKeep = Vector.findIndices predicate vs
          keysToKeep = Index.Internal.fromDistinctAscList [Index.Internal.elemAt ix ks | ix <- Vector.toList indicesToKeep]
       in xs `select` keysToKeep


-- | Filter elements, taking into account the corresponding key. Only elements for which 
-- the predicate is @True@ are kept. 
filterWithKey :: (Vector v a, Vector v Int, Vector v Bool, Ord k) 
              => (k -> a -> Bool) 
              -> Series v k a 
              -> Series v k a
{-# INLINABLE filterWithKey #-}
filterWithKey predicate xs = xs `selectWhere` G.mapWithKey predicate xs


-- | \(O(n)\) Only keep elements which are @'Just' v@. 
catMaybes :: (Vector v a, Vector v (Maybe a), Vector v Int, Ord k) 
       => Series v k (Maybe a) -> Series v k a
{-# INLINABLE catMaybes #-}
catMaybes = G.map fromJust . filter isJust


-- | Datatype representing an /inclusive/ range of keys, which can either be bounded
-- or unbounded. The canonical ways to construct a 'Range' are to use 'to', 'from', and 'upto':
--
-- >>> 'a' `to` 'z'
-- Range (from 'a' to 'z')
-- >>> from 'd'
-- Range (from 'd')
-- >>> upto 'q'
-- Range (up to 'q')
--
-- A 'Range' can be used to efficiently select a sub-series with 'select'.
data Range k 
    = BoundedRange k k
    | From k
    | UpTo k
    deriving (Eq)


instance Show k => Show (Range k) where
    show :: Range k -> String
    show (BoundedRange start stop) = mconcat ["Range (from ", show start, " to ", show stop, ")"]
    show (From start) = mconcat ["Range (from ", show start, ")"]
    show (UpTo stop) = mconcat ["Range (up to ", show stop, ")"]


-- | Find the keys which are in range. In case of an empty 'Series',
-- the returned value is 'Nothing'.
keysInRange :: Ord k => Series v k a -> Range k -> Maybe (k, k)
{-# INLINABLE keysInRange #-}
keysInRange (MkSeries ks _) rng
    = let inrange = inRange rng
       in if Set.null inrange 
            then Nothing
            else Just (Set.findMin inrange, Set.findMax inrange)
    where
        inRange (BoundedRange start stop)  = Set.takeWhileAntitone (<= stop) 
                                           $ Set.dropWhileAntitone (< start) $ Index.toSet ks
        inRange (From start)               = Set.dropWhileAntitone (< start) $ Index.toSet ks
        inRange (UpTo stop)                = Set.takeWhileAntitone (<= stop) $ Index.toSet ks


-- | Create a bounded 'Range' which can be used for slicing. This function
-- is expected to be used in conjunction with 'select'.
--
-- For unbound ranges, see 'from' and 'upto'.
to :: Ord k => k -> k -> Range k
to k1 k2 = BoundedRange (min k1 k2) (max k1 k2)


-- | Create an unbounded 'Range' which can be used for slicing. 
-- This function is expected to be used in conjunction with 'select'. 
--
-- For bound ranges, see 'to'.
from :: k -> Range k
from = From


-- | Create an unbounded 'Range' which can be used for slicing. This function
-- is expected to be used in conjunction with 'select'. 
--
-- For bound ranges, see 'to'.
upto :: k -> Range k
upto = UpTo


-- | Class for datatypes which can be used to select sub-series using 'select'.
--
-- There are two use-cases for 'select':
--
--  * Bulk random-access (selecting from an 'Index' of keys);
--  * Bulk ordered access (selecting from a 'Range' of keys).
--
-- See the documentation for 'select'.
class Selection s where
    -- | Select a subseries. There are two main ways to do this.
    --
    -- The first way to do this is to select a sub-series based on keys:
    --
    -- >>> let xs = Series.fromList [('a', 10::Int), ('b', 20), ('c', 30), ('d', 40)]
    -- >>> xs `select` Index.fromList ['a', 'd']
    -- index | values
    -- ----- | ------
    --   'a' |     10
    --   'd' |     40
    --
    -- The second way to select a sub-series is to select all keys in a range:
    --
    -- >>> xs `select` 'b' `to` 'c'
    -- index | values
    -- ----- | ------
    --   'b' |     20
    --   'c' |     30
    --
    -- Such ranges can also be unbounded. (i.e. all keys smaller or larger than some key), like so:
    --
    -- >>> xs `select` upto 'c'
    -- index | values
    -- ----- | ------
    --   'a' |     10
    --   'b' |     20
    --   'c' |     30
    -- >>> xs `select` from 'c'
    -- index | values
    -- ----- | ------
    --   'c' |     30
    --   'd' |     40
    --
    -- Note that with 'select', you'll always get a sub-series; if you ask for a key which is not
    -- in the series, it'll be ignored:
    --
    -- >>> xs `select` Index.fromList ['a', 'd', 'e']
    -- index | values
    -- ----- | ------
    --   'a' |     10
    --   'd' |     40
    --
    -- See 'require' if you want to ensure that all keys are present.
    select :: (Vector v a, Ord k) => Series v k a -> s k -> Series v k a


instance Selection Index where
    -- | Select all keys in 'Index' from a series. Keys which are not
    -- in the series are ignored.
    select :: (Vector v a, Ord k) => Series v k a -> Index k -> Series v k a
    {-# INLINABLE select #-}
    select xs ss
        = let selectedKeys = index xs `Index.intersection` ss
            -- Surprisingly, using `Vector.backpermute` does not
            -- perform as well as `Vector.map (Vector.unsafeIndex vs)`
            -- for large Series
           in xs `selectSubset` selectedKeys


-- | Selecting a sub-series from a 'Set' is a convenience
-- function. Internally, the 'Set' is converted to an index first.
instance Selection Set where
    select :: (Vector v a, Ord k) => Series v k a -> Set k -> Series v k a
    {-# INLINABLE select #-}
    select xs = select xs . Index.fromSet


-- | Selecting a sub-series from a list is a convenience
-- function. Internally, the list is converted to an index first.
instance Selection [] where
    select :: (Vector v a, Ord k) => Series v k a -> [k] -> Series v k a
    {-# INLINABLE select #-}
    select xs = select xs . Index.fromList


-- | Selecting a sub-series based on a @Range@ is most performant.
-- Constructing a @Range@ is most convenient using the 'to' function.
instance Selection Range where
    select :: (Vector v a, Ord k) => Series v k a -> Range k -> Series v k a
    {-# INLINABLE select #-}
    select series rng = case keysInRange series rng of 
        Nothing              -> mempty
        Just (kstart, kstop) -> let indexOf xs k = Index.Internal.findIndex k (index xs)
                                 in slice (series `indexOf` kstart) (1 + series `indexOf` kstop) series


-- | Select a sub-series from a series matching a condition.
selectWhere :: (Vector v a, Vector v Int, Vector v Bool, Ord k) => Series v k a -> Series v k Bool -> Series v k a
{-# INLINABLE selectWhere #-}
selectWhere xs ys = xs `select` Index.fromSet keysWhereTrue
    where
        (MkSeries _ cond) = ys `select` index xs
        whereValuesAreTrue = Set.fromDistinctAscList $ Vector.toList (Vector.findIndices id cond)
        keysWhereTrue = Set.mapMonotonic (`Index.Internal.elemAt` index xs) whereValuesAreTrue


-- | Implementation of `select` where the selection keys are known
-- to be a subset of the series. This precondition is NOT checked.
--
-- This is a performance optimization and therefore is not normally exposed.
selectSubset :: (Vector v a, Ord k) => Series v k a -> Index k -> Series v k a
{-# INLINABLE selectSubset #-}
selectSubset (MkSeries ks vs) ss
    -- TODO: 
    --   Is it possible to scan over the series once
    --   while filtering away on keys? Initial attempts did not lead
    --   to performance improvements, but I can't imagine that calling
    --   `Index.Internal.findIndex` repeatedly is efficient
    --
    --   Maybe use Data.Series.Index.indexed to traverse the index once?
    = MkSeries ss $ Boxed.convert
                  $ Boxed.map (Vector.unsafeIndex vs . (`Index.Internal.findIndex` ks))
                  $ Index.toAscVector ss


-- | \(O(\log n)\) Yield a subseries based on integer indices. The end index is not included.
slice :: Vector v a
      => Int -- ^ Start index
      -> Int -- ^ End index, which is not included
      -> Series v k a 
      -> Series v k a
{-# INLINABLE slice #-}
slice start stop (MkSeries ks vs) 
    = let stop' = min (Vector.length vs) stop
    -- Index.take is O(log n) while Vector.slice is O(1)
    in MkSeries { index  = Index.take (stop' - start) $ Index.drop start ks
                , values = Vector.slice start (stop' - start) vs
                }


