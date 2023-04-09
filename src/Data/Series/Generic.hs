{-# LANGUAGE NoImplicitPrelude #-}
module Data.Series.Generic (
    -- * Definition
    Series(index, values),
    convert,

    -- * Building/converting `Series`
    -- ** Lists
    fromList, toList,
    -- ** Strict Maps
    fromStrictMap, toStrictMap,
    -- ** Lazy Maps
    fromLazyMap, toLazyMap,

    -- * Mapping and filtering
    map, mapWithKey, mapIndex, filter, length, sum, 

    -- * Combining series
    zipWith, zipWithMatched, 
    ZipStrategy, skipStrategy, constStrategy, zipWithStrategy,

    -- * Index manipulation
    reindex, dropna, dropIndex,

    -- * Accessors
    -- ** Bulk access
    select, selectWhere, Range, to, Selection, 
    -- ** Single-element access
    at, iat, 

    -- * Grouping operations
    GroupBy, groupBy, aggregateWith,

    -- * Numerical aggregations
    mean, nanmean,
    var, nanvar, 
    sampleVariance, nanSampleVariance,
    meanAndVariance, nanMeanAndVariance,
    std, nanstd,

    -- * Windowing operations
    windows, iwindows, expanding, irolling,

    -- * Reading @Series@ from files
    ColumnName, readCSV, readCSVFromFile, columns, columnsFromFile,
    readJSON, readJSONFromFile,
) where

import Data.Series.Generic.Aggregation          ( GroupBy, groupBy, aggregateWith )
import Data.Series.Generic.Broadcast            ( zipWith, zipWithMatched, zipWithStrategy, ZipStrategy, skipStrategy, constStrategy  )
import Data.Series.Generic.Definition           ( Series(index, values), convert, fromStrictMap, toStrictMap, fromLazyMap, toLazyMap, fromList, toList, map, mapWithKey, mapIndex, length, sum )
import Data.Series.Generic.IO                   ( ColumnName, readCSV, readCSVFromFile, columns, columnsFromFile, readJSON, readJSONFromFile )
import Data.Series.Generic.Numeric              ( mean, nanmean, var, nanvar, sampleVariance, nanSampleVariance, std, nanstd, meanAndVariance, nanMeanAndVariance )
import Data.Series.Generic.View                 ( Range, Selection, at, iat, select, selectWhere, to, filter, reindex, dropna, dropIndex )
import Data.Series.Generic.Windowing            ( windows, iwindows, expanding, irolling )