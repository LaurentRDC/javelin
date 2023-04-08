{-# LANGUAGE NoImplicitPrelude #-}
module Data.Series.Generic (
    -- * Definition
    Series(index, values),

    -- * Building/converting @Series@
    fromStrictMap, toStrictMap,
    fromLazyMap, toLazyMap,
    fromList, toList,

    -- * Mapping and filtering
    map, mapWithKey, mapIndex, filter, length, sum, 

    -- * Combining series
    zipWith, zipWithMatched, 
    ZipStrategy, skipStrategy, constStrategy, zipWithStrategy,

    -- * Index manipulation
    reindex, dropna, dropIndex,

    -- * Accessors
    Range, Selection, to, select, selectWhere, at, iat, 

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

    -- * Broadcastable operations
    -- ** Broadcastable operations that may leave holes
    (+:), (-:), (*:), (/:), (==:), (/=:),
    -- ** Broadcastable operations only on matched keys
    (+|), (-|), (*|), (/|), (==|), (/=|),

    -- * Reading @Series@ from files
    ColumnName, readCSV, readCSVFromFile, columns, columnsFromFile,
    readJSON, readJSONFromFile,
) where

import Data.Series.Generic.Aggregation          ( GroupBy, groupBy, aggregateWith )
import Data.Series.Generic.Broadcast            ( zipWith, zipWithMatched, zipWithStrategy, ZipStrategy, skipStrategy, constStrategy  )
import Data.Series.Generic.Broadcast.Drop       ( (+|), (-|), (*|), (/|), (==|), (/=|), )
import Data.Series.Generic.Broadcast.Propagate  ( (+:), (-:), (*:), (/:), (==:), (/=:),)
import Data.Series.Generic.Definition           ( Series(index, values), fromStrictMap, toStrictMap, fromLazyMap, toLazyMap, fromList, toList, map, mapWithKey, mapIndex, length, sum )
import Data.Series.Generic.IO                   ( ColumnName, readCSV, readCSVFromFile, columns, columnsFromFile, readJSON, readJSONFromFile )
import Data.Series.Generic.Numeric              ( mean, nanmean, var, nanvar, sampleVariance, nanSampleVariance, std, nanstd, meanAndVariance, nanMeanAndVariance )
import Data.Series.Generic.View                 ( Range, Selection, at, iat, select, selectWhere, to, filter, reindex, dropna, dropIndex )
import Data.Series.Generic.Windowing            ( windows, iwindows, expanding, irolling )
