{-# LANGUAGE NoImplicitPrelude #-}
module Data.Series.Generic (
    -- * Definition
    Series(index, values),
    convert,

    -- * Building/converting `Series`
    singleton, fromIndex,
    -- ** Lists
    fromList, toList,
    -- ** Strict Maps
    fromStrictMap, toStrictMap,
    -- ** Lazy Maps
    fromLazyMap, toLazyMap,

    -- * Mapping and filtering
    map, mapWithKey, mapIndex, filter, length, sum, 
    takeWhile, dropWhile,

    -- * Combining series
    zipWith, zipWithMatched, 
    ZipStrategy, skipStrategy, constStrategy, zipWithStrategy,

    -- * Index manipulation
    require, requireWith, dropna, dropIndex,

    -- * Accessors
    -- ** Bulk access
    select, selectWhere, Range, to, Selection, 
    -- ** Single-element access
    at, iat, 

    -- * Replacement
    replace, (|->), (<-|),

    -- * Grouping operations
    GroupBy, groupBy, aggregateWith,

    -- * Numerical aggregations
    mean, var, std, 
    sampleVariance,
    meanAndVariance,

    -- * Windowing operations
    windows, iwindows, expanding, irolling,
) where

import Data.Series.Generic.Aggregation  ( GroupBy, groupBy, aggregateWith )
import Data.Series.Generic.Definition   ( Series(index, values), convert, singleton, fromIndex, fromStrictMap, toStrictMap, fromLazyMap, toLazyMap, fromList, toList
                                        , map, mapWithKey, mapIndex, length, sum, takeWhile, dropWhile )
import Data.Series.Generic.Numeric      ( mean, var, sampleVariance, std, meanAndVariance )
import Data.Series.Generic.View         ( Range, Selection, at, iat, select, selectWhere, to, filter, require, requireWith, dropna, dropIndex )
import Data.Series.Generic.Windowing    ( windows, iwindows, expanding, irolling )
import Data.Series.Generic.Zip          ( zipWith, zipWithMatched, replace, (|->), (<-|), zipWithStrategy, ZipStrategy, skipStrategy, constStrategy )
