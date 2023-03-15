
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

    -- * Combining series
    zipWith, zipWithMatched,

    -- * Index manipulation
    reindex, mapIndex, dropna,

    -- * Random access
    at, iat,

    -- * Range access
    Range, to, select, selectWhere,

    -- * Grouping operations
    groupBy, aggregateWith,

    -- * Numerical aggregations
    mean, variance, sampleVariance, std,
    meanAndVariance,

    -- * Windowing operations
    windows, iwindows,

    -- * Broadcastable operations
    -- ** Broadcastable operations that may leave holes
    (+:), (-:), (*:), (/:), (==:), (/=:),
    -- ** Broadcastable operations only on matched keys
    (+|), (-|), (*|), (/|), (==|), (/=|),
) where

import Prelude                hiding ( zipWith )

import Data.Series.Aggregation          ( groupBy, aggregateWith )
import Data.Series.Broadcast            ( zipWith, zipWithMatched,  )
import Data.Series.Broadcast.Drop       ( (+|), (-|), (*|), (/|), (==|), (/=|), )
import Data.Series.Broadcast.Propagate  ( (+:), (-:), (*:), (/:), (==:), (/=:),)
import Data.Series.Definition           ( Series(index), fromStrictMap, toStrictMap, fromLazyMap, toLazyMap, fromList, toList )
import Data.Series.IO                   ( ColumnName, readCSV, readCSVFromFile, columns, columnsFromFile )
import Data.Series.Numeric              ( mean, variance, sampleVariance, std, meanAndVariance )
import Data.Series.View                 ( Range, at, iat, select, selectWhere, to, reindex, mapIndex, dropna)
import Data.Series.Windowing            ( windows, iwindows )
