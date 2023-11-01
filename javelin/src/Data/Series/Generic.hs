{-# LANGUAGE NoImplicitPrelude #-}
module Data.Series.Generic (
    -- * Definition
    Series(index, values),
    convert,

    -- * Building/converting 'Series'
    singleton, fromIndex,
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
    map, mapWithKey, mapIndex, filter, filterWithKey, null, length, sum, 
    take, takeWhile, dropWhile,
    -- ** Mapping with effects
    mapWithKeyM, mapWithKeyM_, forWithKeyM, forWithKeyM_, traverseWithKey,

    -- * Scans
    postscanl, forwardFill,

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
   
    
    -- * Numerical aggregations
    mean, var, std, 
    sampleVariance,
    meanAndVariance,

    -- * Grouping and windowing operations
    groupBy, Grouping, aggregateWith, foldWith, 
    windowing, expanding,
) where

import Data.Series.Generic.Aggregation  ( groupBy, Grouping, aggregateWith, foldWith
                                        , windowing, expanding 
                                        )
import Data.Series.Generic.Definition   ( Series(index, values), Occurrence, convert, singleton, fromIndex, fromStrictMap
                                        , toStrictMap, fromLazyMap, toLazyMap, fromList, fromListDuplicates, toList
                                        , fromVector, fromVectorDuplicates, toVector
                                        , map, mapWithKey, mapIndex, null, length, sum, take, takeWhile, dropWhile
                                        , mapWithKeyM, mapWithKeyM_, forWithKeyM, forWithKeyM_, traverseWithKey
                                        )
import Data.Series.Generic.Numeric      ( mean, var, sampleVariance, std, meanAndVariance )
import Data.Series.Generic.Scans        ( postscanl, forwardFill )
import Data.Series.Generic.View         ( Range, Selection, at, iat, select, selectWhere, to, from, upto, filter, filterWithKey, require, requireWith
                                        , catMaybes, dropIndex, argmax, argmin
                                        )
import Data.Series.Generic.Zip          ( zipWith, zipWithMatched, zipWithKey, zipWith3, zipWithMatched3, zipWithKey3, replace
                                        , (|->), (<-|), zipWithStrategy, zipWithStrategy3, ZipStrategy, skipStrategy, mapStrategy, constStrategy
                                        , zipWithMonoid, esum, eproduct, unzip, unzip3
                                        )
