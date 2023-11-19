-----------------------------------------------------------------------------
-- |
-- Module      :  $header
-- Copyright   :  (c) Laurent P. René de Cotret
-- License     :  MIT-style
-- Maintainer  :  Laurent P. René de Cotret
-- Portability :  portable
--
-- This module contains the definition of 'Index', a sequence of /unique/ and /sorted/
-- keys which can be used to efficient index a 'Data.Series.Series'.
--
-- = Construction
--
-- Constructing an 'Index' can be done from the usual list using `fromList`. Note that 
-- the 'Index' length could be smaller than the input list, due to the requirement that
-- an 'Index' be a sequence of unique keys.  A better way to construct an 'Index' is 
-- to use a 'Data.Set' (`fromSet`)
--
-- For quick inline definitions of an 'Index', you can also make use of the @OverloadedLists@ extension:
-- 
-- >>> :set -XOverloadedLists
-- >>> let (ix :: Index Int) = [1,2,3,4,5,5,5]
-- >>> ix
-- Index [1,2,3,4,5] 
--
-- Another useful function to construct an 'Index' is `range`. This allows to build an 'Index'
-- from a starting value up to an ending value, with a custom step function. For example,
-- here's an 'Index' with values from 1 to 10, in steps of 3:
--
-- >>> range (+3) (1 :: Int) 10
-- Index [1,4,7,10]
--
-- Note that `range` is a special case of the `unfoldr` function, which is also provided in this module.
--
-- = Set operations
-- 
-- Just like a 'Data.Set', 'Index' supports efficient `member`, `notMember`, `union`, `intersection`, and `difference` operations.
-- Like 'Data.Set', the `Semigroup` and `Monoid` instance of 'Index' are defined using the `union` operation:
--
-- >>> fromList ['a', 'b', 'c'] <> fromList ['b', 'c', 'd']
-- Index "abcd"
--
-- = Mapping
--
-- Because of the restriction that all keys be unique, an 'Index' is not a true `Functor`; you can't use
-- `fmap` to map elements of an index. Instead, you can use the general-purpose function 'map'. If you want
-- to map elements of an 'Index' with a monotonic function (i.e. a function which will not re-order elements and won't
-- create duplicate elements), you can use the 'Data.Series.mapMonotonic' function which operates faster.
--
-- = Indexing
--
-- One of the key operations for 'Data.Series.Series' is to find the integer index of an element in an 'Index'. For this purpose, you
-- can use `lookupIndex`:
--
-- >>> lookupIndex 'b' $ fromList ['a', 'b', 'c']
-- Just 1
-- >>> lookupIndex 'd' $ fromList ['a', 'b', 'c']
-- Nothing

module Data.Series.Index (
    Index,

    -- * Creation and Conversion
    singleton,
    unfoldr,
    range,
    fromSet,
    fromList,
    fromVector,
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
    symmetricDifference,
    contains,
    size,
    take,
    drop,

    -- * Mapping and filtering
    map,
    filter,
    traverse,
    
    -- * Indexing
    lookupIndex,

    -- * Insertion and deletion
    insert,
    delete,
) where

import Data.Series.Index.Definition ( Index, singleton, unfoldr, range, fromSet, fromList, fromVector, toSet
                                    , toAscList, toAscVector, null, member, notMember, union, intersection
                                    , difference, symmetricDifference, contains, size, take, drop, map
                                    , filter, traverse, lookupIndex, insert, delete 
                                    )
import Prelude hiding ( null, take, drop, map, filter, traverse )

