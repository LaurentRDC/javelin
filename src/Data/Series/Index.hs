
module Data.Series.Index (
    Index,

    -- * Conversion
    singleton,
    fromSet,
    fromList,
    fromAscList,
    toSet,
    toList,
    toAscList,

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
    
    -- * Indexing
    findIndex,
    lookupIndex,
    elemAt,
) where

import           Control.DeepSeq    (NFData)
import           Data.Set           ( Set )
import qualified Data.Set           as Set
import           Prelude            hiding ( null, take, drop )


newtype Index k = MkIndex (Set k)
    deriving (Eq, Ord, Semigroup, Monoid, Foldable, NFData)


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


-- | \(O(1)\) Convert an `Index` to a `Set`.
toSet :: Index k -> Set k
toSet (MkIndex s) = s


-- | \(O(n)\) Convert an `Index` to a list. Elements will be produced in ascending order. Alias for `toAscList`.
toList :: Index k -> [k]
toList = toAscList


-- | \(O(n)\) Convert an `Index` to a list. Elements will be produced in ascending order.
toAscList :: Index k -> [k]
toAscList (MkIndex s) = Set.toAscList s


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
-- >>> take 10 $ fromList [1,2,3]
-- Index [1,2,3]
take :: Int -> Index k -> Index k
take n (MkIndex ix) = MkIndex (Set.take n ix) 


-- | \(O(\log n)\). Drop @n@ elements from the index, in ascending order.
drop :: Int -> Index k -> Index k
drop n (MkIndex ix) = MkIndex (Set.drop n ix) 


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