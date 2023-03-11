
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

    -- * Combining series
    zipWith, zipWithMatched,

    -- * Random access
    at, iat, select,

    -- * Index manipulation
    reindex,

    -- * Range access
    from, to,

    -- * Numerical aggregations
    mean, variance, sampleVariance, std,
    meanAndVariance,

    -- * Broadcastable operations
    -- ** Broadcastable operations that may leave holes
    (+:), (-:), (*:), (/:),
    -- ** Broadcastable operations only on matched keys
    (+|), (-|), (*|), (/|),
) where

import Prelude                hiding ( zipWith )

import Data.Series.Broadcast            ( zipWith, zipWithMatched,  )
import Data.Series.Broadcast.Drop       ( (+|), (-|), (*|), (/|) )
import Data.Series.Broadcast.Propagate  ( (+:), (-:), (*:), (/:) )
import Data.Series.Conversion           ( fromStrictMap, toStrictMap, fromLazyMap, toLazyMap, fromList, toList )
import Data.Series.Definition           ( Series(index) )
import Data.Series.Numeric              ( mean, variance, sampleVariance, std, meanAndVariance )
import Data.Series.View                 ( at, iat, select, from, to, reindex)
