{-# LANGUAGE NoImplicitPrelude #-}
module Data.Series.Generic (
    -- * Definition
    Series(index, values),
    convert,

    -- * Building/converting 'Series'
    singleton, fromIndex,
    IsSeries(..),
    -- ** Lists
    fromList, toList,
    -- ** Vectors
    fromVector, toVector,
    -- ** Handling duplicates
    Occurrence, fromListDuplicates, fromVectorDuplicates,
    -- ** Strict Maps
    fromStrictMap, toStrictMap,
    -- ** Lazy Maps
    fromLazyMap, toLazyMap,

    -- * Mapping and filtering
    map, mapWithKey, mapIndex, concatMap, filter, filterWithKey, null, length, sum, 
    take, takeWhile, drop, dropWhile,
    -- ** Mapping with effects
    mapWithKeyM, mapWithKeyM_, forWithKeyM, forWithKeyM_, traverseWithKey,

    -- * Folding
    fold, foldM, foldWithKey, foldMWithKey, foldMap, foldMapWithKey,
    mean, variance, std, 

    -- * Scans
    postscanl, prescanl, forwardFill,

    -- * Combining series
    zipWith, zipWithMatched, zipWithKey,
    zipWith3, zipWithMatched3, zipWithKey3,
    ZipStrategy, skipStrategy, mapStrategy, constStrategy, zipWithStrategy, zipWithStrategy3,
    zipWithMonoid, esum, eproduct, unzip, unzip3,

    -- * Index manipulation
    require, requireWith, catMaybes, dropIndex,

    -- * Accessors
    -- ** Bulk access
    select, selectWhere, Range, to, from, upto, Selection, 
    -- ** Single-element access
    at, iat,
    -- ** Finding indices based on values
    argmax, argmin,

    -- * Replacement
    replace, (|->), (<-|),

    -- * Grouping and windowing operations
    groupBy, Grouping, aggregateWith, foldWith, 
    windowing, expanding,
) where

import Data.Series.Generic.Aggregation  ( groupBy, Grouping, aggregateWith, foldWith
                                        , windowing, expanding 
                                        )
import Data.Series.Generic.Definition   ( Series(index, values), IsSeries(..), Occurrence, convert, singleton, fromIndex, fromStrictMap
                                        , toStrictMap, fromLazyMap, toLazyMap, fromList, fromListDuplicates, toList
                                        , fromVector, fromVectorDuplicates, toVector
                                        , map, mapWithKey, mapIndex, concatMap, null, length, sum, take, takeWhile, drop, dropWhile
                                        , mapWithKeyM, mapWithKeyM_, forWithKeyM, forWithKeyM_, traverseWithKey, fold, foldM
                                        , foldWithKey, foldMWithKey, foldMap, foldMapWithKey,
                                        )
import Data.Series.Generic.Numeric      ( mean, variance, std )
import Data.Series.Generic.Scans        ( postscanl, prescanl, forwardFill )
import Data.Series.Generic.View         ( Range, Selection, at, iat, select, selectWhere, to, from, upto, filter, filterWithKey, require, requireWith
                                        , catMaybes, dropIndex, argmax, argmin
                                        )
import Data.Series.Generic.Zip          ( zipWith, zipWithMatched, zipWithKey, zipWith3, zipWithMatched3, zipWithKey3, replace
                                        , (|->), (<-|), zipWithStrategy, zipWithStrategy3, ZipStrategy, skipStrategy, mapStrategy, constStrategy
                                        , zipWithMonoid, esum, eproduct, unzip, unzip3
                                        )
