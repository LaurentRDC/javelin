{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  $header
-- Copyright   :  (c) Laurent P. René de Cotret
-- License     :  MIT-style
-- Maintainer  :  Laurent P. René de Cotret
-- Portability :  portable
--
-- This module contains the definition of 'Index', a sequence of /unique/ and /sorted/
-- keys which can be used to efficient index a 'Series'.


module Data.Series.Index.Definition (
    Index(..),

    -- * Creation and Conversion
    singleton,
    unfoldr,
    range,
    fromSet, toSet,
    fromList, toAscList,
    fromAscList, fromDistinctAscList,
    fromVector, toAscVector,
    fromAscVector, fromDistinctAscVector,
    -- ** Ad-hoc conversion with other data structures
    IsIndex(..),
    
    -- * Set-like operations
    null,
    member,
    notMember,
    union,
    intersection,
    difference,
    symmetricDifference,
    cartesianProduct,
    contains,
    size,
    take,
    drop,

    -- * Mapping and filtering
    map,
    mapMonotonic,
    indexed,
    filter,
    traverse,
    
    -- * Indexing
    findIndex,
    lookupIndex,
    elemAt,

    -- * Insertion and deletion
    insert,
    delete,
) where

import           Control.DeepSeq        ( NFData )
import           Control.Monad          ( guard )
import           Control.Monad.ST       ( runST )
import           Data.Coerce            ( coerce )
import qualified Data.Foldable          as Foldable
import           Data.Functor           ( ($>) )
import           Data.IntSet            ( IntSet )
import qualified Data.IntSet            as IntSet
import qualified Data.List              as List
import           Data.Sequence          ( Seq )
import qualified Data.Sequence          as Seq
import           Data.Set               ( Set )
import qualified Data.Set               as Set
import qualified Data.Traversable       as Traversable
import qualified Data.Vector            as Boxed
import           Data.Vector.Algorithms.Intro ( sortUniq )
import           Data.Vector.Generic    ( Vector )
import qualified Data.Vector.Generic    as Vector
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed    as Unboxed
import           GHC.Exts               ( IsList )
import qualified GHC.Exts               as Exts
import           GHC.Stack              ( HasCallStack )
import           Prelude                as P hiding ( null, take, drop, map, filter, traverse, product )

-- $setup
-- >>> import Data.Series.Index
-- >>> import qualified Data.Vector as Vector


-- | Representation of the index of a series.
-- An index is a sequence of sorted elements. All elements are unique, much like a 'Set'.
--
-- You can construct an 'Index' from a set ('fromSet'), from a list ('fromList'), or from a vector ('fromVector'). You can 
-- also make use of the @OverloadedLists@ extension:
--
-- >>> :set -XOverloadedLists
-- >>> let (ix :: Index Int) = [1, 2, 3]
-- >>> ix
-- Index [1,2,3]
--
-- Since keys in an 'Index' are always sorted and unique, 'Index' is not a 'Functor'. To map a function
-- over an 'Index', use 'map'.
newtype Index k = MkIndex (Set k)
    deriving (Eq, Ord, Semigroup, Monoid, Foldable, NFData)


instance Ord k => IsList (Index k) where
    type Item (Index k) = k
    fromList :: [k] -> Index k
    fromList = fromList
    toList :: Index k -> [Exts.Item (Index k)]
    toList = toAscList


instance Show k => Show (Index k) where
    show :: Index k -> String
    show (MkIndex s) = "Index " ++ show (Set.toList s)


-- | \(O(1)\)  Create a singleton 'Index'.
singleton :: k -> Index k
singleton = MkIndex . Set.singleton
{-# INLINABLE singleton #-}


-- | \(O(n \log n)\) Create an 'Index' from a seed value. 
-- Note that the order in which elements are generated does not matter; elements are stored
-- in order. See the example below.
--
-- >>> unfoldr (\x -> if x < 1 then Nothing else Just (x, x-1)) (7 :: Int)
-- Index [1,2,3,4,5,6,7]
unfoldr :: Ord a => (b -> Maybe (a, b)) -> b -> Index a
unfoldr f = fromList . List.unfoldr f
{-# INLINABLE unfoldr #-}


-- | \(O(n \log n)\) Create an 'Index' as a range of values. @range f start end@ will generate 
-- an 'Index' with values @[start, f start, f (f start), ... ]@ such that the largest element
-- less or equal to @end@ is included. See examples below.
--
-- >>> range (+3) (1 :: Int) 10
-- Index [1,4,7,10]
-- >>> range (+3) (1 :: Int) 11
-- Index [1,4,7,10]
range :: Ord a 
      => (a -> a) -- ^ Function to generate the next element in the index
      -> a        -- ^ Starting value of the 'Index'
      -> a        -- ^ Ending value of the 'Index', which may or may not be contained
      -> Index a
range next start end 
    = unfoldr (\x -> guard (x <= end) $> (x, next x)) start
{-# INLINABLE range #-}


-- | The 'IsIndex' typeclass allow for ad-hoc definition
-- of conversion functions, converting to / from 'Index'.
class IsIndex t k where
    -- | Construct an 'Index' from some container of keys. There is no
    -- condition on the order of keys. Duplicate keys are silently dropped.
    toIndex    :: t -> Index k

    -- | Construct a container from keys of an 'Index'. 
    -- The elements are returned in ascending order of keys.
    fromIndex  :: Index k -> t


instance IsIndex (Set k) k where
    -- | \(O(1)\) Build an 'Index' from a 'Set'.
    toIndex :: Set k -> Index k
    toIndex = coerce
    {-# INLINABLE toIndex #-}

    -- | \(O(1)\) Build an 'Index' from a 'Set'.
    fromIndex :: Index k -> Set k
    fromIndex = coerce
    {-# INLINABLE fromIndex #-}


instance Ord k => IsIndex [k] k where
    -- | \(O(n \log n)\) Build an 'Index' from a list.
    toIndex :: [k] -> Index k
    toIndex = fromList
    {-# INLINABLE toIndex #-}

    -- | \(O(n)\) Convert an 'Index' to a list.
    fromIndex :: Index k -> [k]
    fromIndex = toAscList
    {-# INLINABLE fromIndex #-}


instance Ord k => IsIndex (Seq k) k where
    -- | \(O(n \log n)\) Build an 'Index' from a 'Seq'.
    toIndex :: Seq k -> Index k
    toIndex = fromList . Foldable.toList
    {-# INLINABLE toIndex #-}

    -- | \(O(n)\) Convert an 'Index' to a 'Seq'.
    fromIndex :: Index k -> Seq k
    fromIndex = Seq.fromList . toAscList
    {-# INLINABLE fromIndex #-}


instance IsIndex IntSet Int where
    -- | \(O(n \min(n,W))\), where \W\ is the number of bits in an 'Int' on your platform (32 or 64).
    toIndex :: IntSet -> Index Int
    toIndex = fromDistinctAscList . IntSet.toList
    {-# INLINABLE toIndex #-}
    
    -- | \(O(n)\) Convert an 'Index' to an 'IntSet.
    fromIndex :: Index Int -> IntSet
    fromIndex = IntSet.fromDistinctAscList . toAscList
    {-# INLINABLE fromIndex #-}


instance (Ord k) => IsIndex (Boxed.Vector k) k where
    toIndex :: Boxed.Vector k -> Index k
    toIndex = fromVector
    {-# INLINABLE toIndex #-} 

    fromIndex :: Index k -> Boxed.Vector k
    fromIndex = toAscVector
    {-# INLINABLE fromIndex #-}


instance (Ord k, Unboxed.Unbox k) => IsIndex (Unboxed.Vector k) k where
    toIndex :: Unboxed.Vector k -> Index k
    toIndex = fromVector
    {-# INLINABLE toIndex #-} 

    fromIndex :: Index k -> Unboxed.Vector k
    fromIndex ix = runST $ M.generate (size ix) (`elemAt` ix) >>= Vector.freeze
    {-# INLINABLE fromIndex #-}


-- | \(O(1)\) Build an 'Index' from a 'Set'.
fromSet :: Set k -> Index k
fromSet = toIndex
{-# INLINABLE fromSet #-}


-- | \(O(n \log n)\) Build an 'Index' from a list. Note that since an 'Index' is
-- composed of unique elements, the length of the index may not be
-- the same as the length of the input list:
--
-- >>> fromList ['c', 'a', 'b', 'b']
-- Index "abc"
--
-- If the list is already sorted, `fromAscList` is generally faster.
fromList :: Ord k => [k] -> Index k
fromList = fromSet . Set.fromList
{-# INLINABLE fromList #-}


-- | \(O(n)\) Build an 'Index' from a list of elements in ascending order. The precondition
-- that elements already be sorted is not checked.
-- 
-- Note that since an 'Index' is composed of unique elements, the length of 
-- the index may not be the same as the length of the input list.
fromAscList :: Eq k => [k] -> Index k
fromAscList = toIndex . Set.fromAscList
{-# INLINABLE fromAscList #-}


-- | \(O(n)\) Build an 'Index' from a list of distinct elements in ascending order. The precondition
-- that elements be unique and sorted is not checked.
fromDistinctAscList :: [k] -> Index k
fromDistinctAscList = MkIndex . Set.fromDistinctAscList
{-# INLINABLE fromDistinctAscList #-}


-- | \(O(n \log n)\) Build an 'Index' from a 'Vector'. Note that since an 'Index' is
-- composed of unique elements, the length of the index may not be
-- the same as the length of the input vector:
--
-- >>> import Data.Vector as V
-- >>> fromVector $ V.fromList ['c', 'a', 'b', 'b']
-- Index "abc"
--
-- If the 'Vector' is already sorted, 'fromAscVector' is generally faster.
fromVector :: (Vector v k, Ord k) => v k -> Index k
fromVector vs = fromDistinctAscVector $ runST $ Vector.thaw vs >>= sortUniq >>= Vector.freeze
{-# INLINABLE fromVector #-}


-- | \(O(n \log n)\) Build an 'Index' from a 'Vector' of elements in ascending order. The precondition
-- that elements already be sorted is not checked. 
--
-- Note that since an 'Index' is composed of unique elements, 
-- the length of the index may not be the same as the length of the input vector:
--
-- >>> import Data.Vector as V
-- >>> fromAscVector $ V.fromList ['a', 'b', 'b', 'c']
-- Index "abc"
fromAscVector :: (Vector v k, Ord k) => v k -> Index k
fromAscVector = fromAscList . Vector.toList
{-# INLINABLE fromAscVector #-}


-- | \(O(n)\) Build an 'Index' from a 'Vector' of unique elements in ascending order. The precondition
-- that elements already be unique and sorted is not checked.
fromDistinctAscVector :: Vector v k => v k -> Index k
fromDistinctAscVector = fromDistinctAscList . Vector.toList
{-# INLINABLE fromDistinctAscVector #-}


-- | \(O(1)\) Convert an 'Index' to a 'Set'.
toSet :: Index k -> Set k
toSet = fromIndex
{-# INLINABLE toSet #-}


-- | \(O(n)\) Convert an 'Index' to a list. Elements will be produced in ascending order.
toAscList :: Index k -> [k]
toAscList (MkIndex s) = Set.toAscList s
{-# INLINABLE toAscList #-}


-- | \(O(n)\) Convert an 'Index' to a list. Elements will be produced in ascending order.
toAscVector :: Vector v k => Index k -> v k
toAscVector ix = runST $ M.generate (size ix) (`elemAt` ix) >>= Vector.freeze
{-# INLINABLE toAscVector #-}


-- | \(O(1)\) Returns 'True' for an empty 'Index', and @False@ otherwise.
null :: Index k -> Bool
null (MkIndex ix) = Set.null ix
{-# INLINABLE null #-}


-- | \(O(n \log n)\) Check whether the element is in the index.
member :: Ord k => k -> Index k -> Bool
member k (MkIndex ix) = k `Set.member` ix
{-# INLINABLE member #-}


-- | \(O(n \log n)\) Check whether the element is NOT in the index.
notMember :: Ord k => k -> Index k -> Bool
notMember k = not . member k
{-# INLINABLE notMember #-}


-- | \(O\bigl(m \log\bigl(\frac{n+1}{m+1}\bigr)\bigr), \; m \leq n\) Union of two 'Index', containing
-- elements either in the left index, right right index, or both.
union :: Ord k => Index k -> Index k -> Index k
union = (<>)
{-# INLINABLE union #-}


-- | \(O\bigl(m \log\bigl(\frac{n+1}{m+1}\bigr)\bigr), \; m \leq n\) Intersection of two 'Index', containing
-- elements which are in both the left index and the right index.
--
-- Elements of the result come from the first 'Index':
--
-- >>> import Data.Semigroup ( Arg(..) )
-- >>> fromList [ Arg 0 'a' ] `intersection` fromList [ Arg 0 'b', Arg 1 'c' ]
-- Index [Arg 0 'a']
intersection :: Ord k => Index k -> Index k -> Index k
intersection (MkIndex ix) (MkIndex jx) = MkIndex $ ix `Set.intersection` jx
{-# INLINABLE intersection #-}


-- | \(O\bigl(m \log\bigl(\frac{n+1}{m+1}\bigr)\bigr), \; m \leq n\) Returns the elements of the first index 
-- which are not found in the second index.
--
-- >>> difference (fromList ['a', 'b', 'c']) (fromList ['b', 'c', 'd'])
-- Index "a"
difference :: Ord k => Index k -> Index k -> Index k
difference (MkIndex ix) (MkIndex jx) = MkIndex $ Set.difference ix jx
{-# INLINABLE difference #-}


-- | \(O(n+m)\). The symmetric difference of two 'Index'.
-- The first element of the tuple is an 'Index' containing all elements which
-- are only found in the left 'Index', while the second element of the tuple is an 'Index' containing
-- all elements which are only found in the right 'Index':
--
-- >>> left = fromList ['a', 'b', 'c']
-- >>> right = fromList ['c', 'd', 'e']
-- >>> left `symmetricDifference` right
-- (Index "ab",Index "de")
symmetricDifference :: Ord k => Index k -> Index k -> (Index k, Index k)
symmetricDifference left right = (left `difference` right, right `difference` left)
{-# INLINABLE symmetricDifference #-}


-- | \(O(n m)\) Take the cartesian product of two 'Index':
--
-- >>> (range (+1) (1 :: Int) 2) `cartesianProduct` (range (+1) (3 :: Int) 4)
-- Index [(1,3),(1,4),(2,3),(2,4)]
cartesianProduct :: Index k -> Index g -> Index (k, g)
cartesianProduct (MkIndex xs) (MkIndex ys) 
    = MkIndex $ Set.cartesianProduct xs ys
{-# INLINABLE cartesianProduct #-}


-- | \(O\bigl(m \log\bigl(\frac{n+1}{m+1}\bigr)\bigr), \; m \leq n\).
-- @(ix1 \'contains\' ix2)@ indicates whether all keys in @ix2@ are also in @ix1@.
contains :: Ord k => Index k -> Index k -> Bool
contains (MkIndex ix1) (MkIndex ix2)= ix2 `Set.isSubsetOf` ix1
{-# INLINABLE contains #-}


-- | \(O(1)\) Returns the number of keys in the index.
size :: Index k -> Int
size (MkIndex ix) = Set.size ix
{-# INLINABLE size #-}


-- | \(O(\log n)\). Take @n@ elements from the index, in ascending order.
-- Taking more than the number of elements in the index is a no-op:
--
-- >>> take 10 $ fromList [1::Int,2,3]
-- Index [1,2,3]
take :: Int -> Index k -> Index k
take n (MkIndex ix) = MkIndex (Set.take n ix)
{-# INLINABLE take #-}


-- | \(O(\log n)\). Drop @n@ elements from the index, in ascending order.
drop :: Int -> Index k -> Index k
drop n (MkIndex ix) = MkIndex (Set.drop n ix)
{-# INLINABLE drop #-}


-- | \(O(n \log n)\) Map a function over keys in the index.
-- Note that since keys in an 'Index' are unique, the length of the resulting
-- index may not be the same as the input:
--
-- >>> map (\x -> if even x then 0::Int else 1) $ fromList [0::Int,1,2,3,4]
-- Index [0,1]
--
-- If the mapping is monotonic, see 'mapMonotonic', which has better performance
-- characteristics.
map :: Ord g => (k -> g) -> Index k -> Index g
map f (MkIndex ix) = MkIndex $ Set.map f ix
{-# INLINABLE map #-}


-- | \(O(n)\) Map a monotonic function over keys in the index. /Monotonic/ means that if @a < b@, then @f a < f b@.
-- Using 'mapMonononic' can be much faster than using 'map' for a large 'Index'.
--
-- >>> mapMonotonic (+1) $ fromList [0::Int,1,2,3,4,5]
-- Index [1,2,3,4,5,6]
-- 
-- The precondiction that the function be monotonic __is not checked__. Using 'mapMonotonic'
-- with a non-monotonic function will __not__ result in an exception; subtle inconsistencies will make you
-- question your own sanity.
mapMonotonic :: (k -> g) -> Index k -> Index g
mapMonotonic f (MkIndex ix) = MkIndex $ Set.mapMonotonic f ix
{-# INLINABLE mapMonotonic #-}


-- | \(O(n)\) Pair each key in the index with its position in the index, starting with 0:
--
-- @since 0.1.1.0
--
-- >>> indexed (fromList ['a', 'b', 'c', 'd'])
-- Index [(0,'a'),(1,'b'),(2,'c'),(3,'d')]
indexed :: Index k -> Index (Int, k)
indexed = fromDistinctAscList 
        . zip [0..] 
        . toAscList
{-# INLINABLE indexed #-}


-- | \(O(n)\) Filter elements satisfying a predicate.
--
-- >>> filter even $ fromList [1::Int,2,3,4,5]
-- Index [2,4]
filter :: (k -> Bool) -> Index k -> Index k
filter p (MkIndex ix) = MkIndex $ Set.filter p ix
{-# INLINABLE filter #-}


-- | \(O(\log n)\). Returns the integer /index/ of a key. This function raises an exception
-- if the key is not in the 'Index'; see 'lookupIndex' for a safe version.
--
-- >>> findIndex 'b' $ fromList ['a', 'b', 'c']
-- 1
findIndex :: HasCallStack => Ord k => k -> Index k -> Int
findIndex e (MkIndex ix) = Set.findIndex e ix 
{-# INLINABLE findIndex #-}


-- | \(O(\log n)\). Returns the integer /index/ of a key, if the key is in the index.
--
-- >>> lookupIndex 'b' $ fromList ['a', 'b', 'c']
-- Just 1
-- >>> lookupIndex 'd' $ fromList ['a', 'b', 'c']
-- Nothing
lookupIndex :: Ord k => k -> Index k -> Maybe Int
lookupIndex e (MkIndex ix) = Set.lookupIndex e ix
{-# INLINABLE lookupIndex #-}


-- | \(O(\log n)\) Returns the element at some integer index. 
-- 
-- This function raises an exception if the integer index is out-of-bounds. 
-- Consider using 'lookupIndex' instead.
elemAt :: HasCallStack => Int -> Index k -> k
elemAt n (MkIndex ix) = Set.elemAt n ix
{-# INLINABLE elemAt #-}


-- | \(O(\log n)\). Insert a key in an 'Index'. If the key is already 
-- present, the 'Index' will not change.
insert :: Ord k => k -> Index k -> Index k
insert k (MkIndex ix) = MkIndex $ k `Set.insert` ix
{-# INLINABLE insert #-}


-- | \(O(\log n)\). Delete a key from an 'Index', if this key is present
-- in the index.
delete :: Ord k => k -> Index k -> Index k
delete k (MkIndex ix) = MkIndex $ k `Set.delete` ix
{-# INLINABLE delete #-}


-- | \(O(n \log n)\). Map each element of an 'Index' to an applicative action, 
-- evaluate these actions from left to right, and collect the results.
--
-- Note that the data type 'Index' is not a member of 'Traversable'
-- because it is not a 'Functor'.
traverse :: (Applicative f, Ord b) => (k -> f b) -> Index k -> f (Index b)
traverse f = fmap fromList . Traversable.traverse f . toAscList
{-# INLINABLE traverse #-}
