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
    map, mapWithKey, mapIndex, filter, null, length, sum, 
    take, takeWhile, dropWhile,
    -- ** Mapping with effects
    mapWithKeyM, mapWithKeyM_, forWithKeyM, forWithKeyM_,

    -- * Scans
    postscanl,

    -- * Combining series
    zipWith, zipWithMatched, zipWithIndex,
    ZipStrategy, skipStrategy, mapStrategy, constStrategy, zipWithStrategy,
    zipWithMonoid, esum, eproduct,

    -- * Index manipulation
    require, requireWith, dropna, dropIndex,

    -- * Accessors
    -- ** Bulk access
    select, selectWhere, Range, to, Selection, 
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
    groupBy, foldGroups, 
    windowing, expanding,
) where

import Data.Series.Generic.Aggregation  ( groupBy, foldGroups
                                        , windowing, expanding 
                                        )
import Data.Series.Generic.Definition   ( Series(index, values), Occurrence, convert, singleton, fromIndex, fromStrictMap
                                        , toStrictMap, fromLazyMap, toLazyMap, fromList, fromListDuplicates, toList
                                        , fromVector, fromVectorDuplicates, toVector
                                        , map, mapWithKey, mapIndex, null, length, sum, take, takeWhile, dropWhile
                                        , mapWithKeyM, mapWithKeyM_, forWithKeyM, forWithKeyM_
                                        )
import Data.Series.Generic.Numeric      ( mean, var, sampleVariance, std, meanAndVariance )
import Data.Series.Generic.Scans        ( postscanl )
import Data.Series.Generic.View         ( Range, Selection, at, iat, select, selectWhere, to, filter, require, requireWith, dropna, dropIndex, argmax, argmin, )
import Data.Series.Generic.Zip          ( zipWith, zipWithMatched, zipWithIndex, replace, (|->), (<-|), zipWithStrategy, ZipStrategy, skipStrategy, mapStrategy, constStrategy
                                        , zipWithMonoid, esum, eproduct
                                        )
