
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
    from, to
) where

import Data.Series.Conversion (fromStrictMap, toStrictMap, fromLazyMap, toLazyMap, fromList, toList)
import Data.Series.Definition ( Series(index) )
import Data.Series.View       ( at, iat, select, from, to, reindex)
