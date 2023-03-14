
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
    ColumnName, readCSV, readCSVFromFile,

    -- * Combining series
    zipWith, zipWithMatched,

    -- * Index manipulation
    reindex, mapIndex,

    -- * Random access
    at, iat,

    -- * Range access
    to, select,

    -- * Grouping operations
    groupBy, aggregateWith,

    -- * Numerical aggregations
    mean, variance, sampleVariance, std,
    meanAndVariance,

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
import Data.Series.IO                   ( ColumnName, readCSV, readCSVFromFile )
import Data.Series.Numeric              ( mean, variance, sampleVariance, std, meanAndVariance )
import Data.Series.View                 ( at, iat, select, to, reindex, mapIndex)
