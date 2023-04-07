{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  $header
-- Copyright   :  (c) Laurent P. René de Cotret
-- License     :  MIT-style
-- Maintainer  :  Laurent P. René de Cotret
-- Portability :  portable
--
-- This module contains the definition of `Index`, a sequence of /unique/ and /sorted/
-- keys which can be used to efficient index a `Series`.
--
-- = Construction
--
-- Constructing an `Index` can be done from the usual list using `fromList`. Note that 
-- the `Index` length could be smaller than the input list, due to the requirement that
-- an `Index` be a sequence of unique keys.  A better way to construct an `Index` is 
-- to use a `Set` (`fromSet`)
--
-- For quick inline definitions of an `Index`, you can also make use of the @OverloadedLists@ extension:
-- 
-- >>> :set -XOverloadedLists
-- >>> let (ix :: Index Int) = [1,2,3,4,5,5,5]
-- >>> ix
-- Index [1,2,3,4,5] 
--
-- = Set operations
-- 
-- Just like a `Set`, `Index` supports efficient `member`, `notMember`, `union`, `intersection`, and `difference` operations.
-- Like `Set`, the `Semigroup` and `Monoid` instance of `Index` are defined using the `union` operation:
--
-- >>> fromList ['a', 'b', 'c'] <> fromList ['b', 'c', 'd']
-- Index "abcd"
--
-- = Mapping
--
-- Because of the restriction that all keys be unique, an `Index` is not a true `Functor`; you can't use
-- `fmap` to map elements of an index. Instead, you can use the general-purpose function `map`. If you want
-- to map elements of an `Index` with a monotonic function (i.e. a function which will not re-order elements and won't
-- create duplicate elements), you can use the `mapMonotonic` function which operates faster.
--
-- = Indexing
--
-- One of the key operations for `Series` is to find the integer index of an element in an `Index`. For this purpose, you
-- can use `lookupIndex`:
--
-- >>> lookupIndex 'b' $ fromList ['a', 'b', 'c']
-- Just 1
-- >>> lookupIndex 'd' $ fromList ['a', 'b', 'c']
-- Nothing

module Data.Series.Index (
    Index,

    -- * Conversion
    singleton,
    fromSet,
    fromList,
    fromAscList,
    fromVector,
    fromAscVector,
    toSet,
    toAscList,
    toAscVector,

    -- * Set-like operations
    null,
    member,
    notMember,
    union,
    intersection,
    difference,
    size,
    take,
    drop,

    -- * Mapping and filtering
    map,
    mapMonotonic,
    filter,
    
    -- * Indexing
    findIndex,
    lookupIndex,
    elemAt,
) where

import           Control.DeepSeq    (NFData)
import           Data.Set           ( Set )
import qualified Data.Set           as Set
import           Data.Vector        ( Vector )
import qualified Data.Vector        as Vector
import           GHC.Exts           ( IsList )
import qualified GHC.Exts           as Exts
import           Prelude            hiding ( null, take, drop, map, filter )


-- | Representation of the index of a series.
-- An index is a sequence of sorted elements. All elements are unique, much like a `Set`.
--
-- You can construct an `Index` from a set (`fromSet`) or from a list (`fromList`). You can 
-- also make use of the @OverloadedLists@ extension:
--
-- >>> :set -XOverloadedLists
-- >>> let (ix :: Index Int) = [1, 2, 3]
-- >>> ix
-- Index [1,2,3]
--
-- Since keys in an `Index` are always sorted and unique, `Index` is not a `Functor`. To map a function
-- over an `Index`, use `map`.
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


-- | \(O(1)\)  Create a singleton `Index`.
singleton :: k -> Index k
singleton = MkIndex . Set.singleton 


-- | \(O(1)\) Build an `Index` from a `Set`.
fromSet :: Set k -> Index k
fromSet = MkIndex 


-- | \(O(n \log n)\) Build an `Index` from a list. Note that since an `Index` is
-- composed of unique elements, the length of the index may not be
-- the same as the length of the input list:
--
-- >>> fromList ['c', 'a', 'b', 'b']
-- Index "abc"
--
-- If the list is already sorted, `fromAscList` is generally faster.
fromList :: Ord k => [k] -> Index k
fromList = fromSet . Set.fromList


-- | \(O(n)\) Build an `Index` from a list of elements in ascending order. The precondition
-- that elements already be sorted is not checked.
-- 
-- Note that since an `Index` is composed of unique elements, the length of 
-- the index may not be the same as the length of the input list.
fromAscList :: Eq k => [k] -> Index k
fromAscList = MkIndex . Set.fromAscList


-- | \(O(n \log n)\) Build an `Index` from a `Vector`. Note that since an `Index` is
-- composed of unique elements, the length of the index may not be
-- the same as the length of the input vector:
--
-- >>> import Data.Vector as Vector
-- >>> fromVector $ Vector.fromList ['c', 'a', 'b', 'b']
-- Index "abc"
--
-- If the `Vector` is already sorted, `fromAscVector` is generally faster.
fromVector :: Ord k => Vector k -> Index k
fromVector = fromList . Vector.toList


-- | \(O(n \log n)\) Build an `Index` from a `Vector` of elements in ascending order. The precondition
-- that elements already be sorted is not checked. 
--
-- Note that since an `Index` is composed of unique elements, 
-- the length of the index may not be the same as the length of the input vector:
--
-- >>> import Data.Vector as Vector
-- >>> fromAscVector $ Vector.fromList ['a', 'b', 'b', 'c']
-- Index "abc"
fromAscVector :: Ord k => Vector k -> Index k
fromAscVector = fromAscList . Vector.toList


-- | \(O(1)\) Convert an `Index` to a `Set`.
toSet :: Index k -> Set k
toSet (MkIndex s) = s


-- | \(O(n)\) Convert an `Index` to a list. Elements will be produced in ascending order.
toAscList :: Index k -> [k]
toAscList (MkIndex s) = Set.toAscList s


-- | \(O(n)\) Convert an `Index` to a list. Elements will be produced in ascending order.
toAscVector :: Index k -> Vector k
toAscVector ix = Vector.fromListN (size ix) $ toAscList ix


-- | \(O(1)\) Returns @True@ for an empty `Index`, and @False@ otherwise.
null :: Index k -> Bool
null (MkIndex ix) = Set.null ix


-- | \(O(n \log n)\) Check whether the element is in the index.
member :: Ord k => k -> Index k -> Bool
member k (MkIndex ix) = k `Set.member` ix


-- | \(O(n \log n)\) Check whether the element is NOT in the index.
notMember :: Ord k => k -> Index k -> Bool
notMember k = not . member k


-- | \(O\bigl(m \log\bigl(\frac{n+1}{m+1}\bigr)\bigr), \; m \leq n\) Union of two `Index`, containing
-- elements either in the left index, right right index, or both.
union :: Ord k => Index k -> Index k -> Index k
union = (<>)


-- | \(O\bigl(m \log\bigl(\frac{n+1}{m+1}\bigr)\bigr), \; m \leq n\) Intersection of two `Index`, containing
-- elements which are in both the left index and the right index.
intersection :: Ord k => Index k -> Index k -> Index k
intersection (MkIndex ix) (MkIndex jx) = MkIndex $ ix `Set.intersection` jx


-- | \(O\bigl(m \log\bigl(\frac{n+1}{m+1}\bigr)\bigr), \; m \leq n\) Returns the elements of the first index 
-- which are not found in the second index.
--
-- >>> difference (fromList ['a', 'b', 'c']) (fromList ['b', 'c', 'd'])
-- Index "a"
difference :: Ord k => Index k -> Index k -> Index k
difference (MkIndex ix) (MkIndex jx) = MkIndex $ Set.difference ix jx


-- | \(O(1)\) Returns the number of keys in the index.
size :: Index k -> Int
size (MkIndex ix) = Set.size ix


-- | \(O(\log n)\). Take @n@ elements from the index, in ascending order.
-- Taking more than the number of elements in the index is a no-op:
--
-- >>> take 10 $ fromList [1::Int,2,3]
-- Index [1,2,3]
take :: Int -> Index k -> Index k
take n (MkIndex ix) = MkIndex (Set.take n ix) 


-- | \(O(\log n)\). Drop @n@ elements from the index, in ascending order.
drop :: Int -> Index k -> Index k
drop n (MkIndex ix) = MkIndex (Set.drop n ix) 


-- | \(O(n \log n)\) Map a function over keys in the index.
-- Note that since keys in an `Index` are unique, the length of the resulting
-- index may not be the same as the input:
--
-- >>> map (\x -> if even x then 0::Int else 1) $ fromList [0::Int,1,2,3,4]
-- Index [0,1]
--
-- If the mapping is monotonic, see `mapMonotonic`, which has better performance
-- characteristics.
map :: Ord g => (k -> g) -> Index k -> Index g
map f (MkIndex ix) = MkIndex $ Set.map f ix


-- | \(O(n)\) Map a monotonic function over keys in the index. /Monotonic/ means that if @a < b@, then @f a < f b@.
-- Using `mapMonononic` can be much faster than using `map` for a large `Index`.
-- Note that the precondiction that the function be monotonic is not checked.
--
-- >>> mapMonotonic (+1) $ fromList [0::Int,1,2,3,4,5]
-- Index [1,2,3,4,5,6]
mapMonotonic :: (k -> g) -> Index k -> Index g
mapMonotonic f (MkIndex ix) = MkIndex $ Set.mapMonotonic f ix


-- | \(O(n)\) Filter elements satisfying a predicate.
--
-- >>> filter even $ fromList [1::Int,2,3,4,5]
-- Index [2,4]
filter :: (k -> Bool) -> Index k -> Index k
filter p (MkIndex ix) = MkIndex $ Set.filter p ix


-- | \(O(\log n)\). Returns the integer /index/ of a key. This function raises an exception
-- if the key is not in the `Index`; see `lookupIndex` for a safe version.
--
-- >>> findIndex 'b' $ fromList ['a', 'b', 'c']
-- 1
findIndex :: Ord k => k -> Index k -> Int
findIndex e (MkIndex ix) = Set.findIndex e ix 


-- | \(O(\log n)\). Returns the integer /index/ of a key, if the key is in the index.
--
-- >>> lookupIndex 'b' $ fromList ['a', 'b', 'c']
-- Just 1
-- >>> lookupIndex 'd' $ fromList ['a', 'b', 'c']
-- Nothing
lookupIndex :: Ord k => k -> Index k -> Maybe Int
lookupIndex e (MkIndex ix) = Set.lookupIndex e ix 


-- | \(O(\log n)\) Returns the element at some integer index. This function raises
-- an exception if the integer index is out-of-bounds.
elemAt :: Int -> Index k -> k
elemAt n (MkIndex ix) = Set.elemAt n ix
