
module Data.Series (
    -- * Definition
    Series(index),

    -- * Building/converting @Series@
    fromStrictMap,
    toStrictMap,
    fromLazyMap,
    toLazyMap,
    fromList,
    toList,

    -- * Reading @Series@ from files
    ColumnName, readCSV, readCSVFromFile, columns, columnsFromFile,
    readJSON, readJSONFromFile,

    -- * Combining series
    zipWith, zipWithMatched,

    -- * Index manipulation
    reindex, mapIndex, dropna, dropIndex,

    -- * Random access
    at, iat,

    -- * Range access
    Range, Selection, to, select, selectWhere,

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
) where

import Prelude                hiding ( zipWith )

import Data.Series.Aggregation          ( GroupBy, groupBy, aggregateWith )
import Data.Series.Broadcast            ( zipWith, zipWithMatched,  )
import Data.Series.Broadcast.Drop       ( (+|), (-|), (*|), (/|), (==|), (/=|), )
import Data.Series.Broadcast.Propagate  ( (+:), (-:), (*:), (/:), (==:), (/=:),)
import Data.Series.Definition           ( Series(index), fromStrictMap, toStrictMap, fromLazyMap, toLazyMap, fromList, toList )
import Data.Series.IO                   ( ColumnName, readCSV, readCSVFromFile, columns, columnsFromFile, readJSON, readJSONFromFile )
import Data.Series.Numeric              ( mean, nanmean, var, nanvar, sampleVariance, nanSampleVariance, std, nanstd, meanAndVariance, nanMeanAndVariance )
import Data.Series.View                 ( Range, Selection, at, iat, select, selectWhere, to, reindex, mapIndex, dropna, dropIndex )
import Data.Series.Windowing            ( windows, iwindows, expanding, irolling )
