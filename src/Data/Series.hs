
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

    -- * Random access
    at, iat, select,

    -- * Index manipulation
    reindex,

    -- * Range access
    from, to,

    -- * Numerical aggregations
    mean, variance, sampleVariance, std
) where

import Data.Series.Conversion (fromStrictMap, toStrictMap, fromLazyMap, toLazyMap, fromList, toList)
import Data.Series.Definition ( Series(index) )
import Data.Series.Numeric    ( mean, variance, sampleVariance, std )
import Data.Series.View       ( at, iat, select, from, to, reindex)
