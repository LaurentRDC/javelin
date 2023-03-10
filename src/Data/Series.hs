
module Data.Series (
    -- * Definition
    Series(index, values),

    -- * Building/converting @Series@
    fromStrictMap,
    toStrictMap,
    fromLazyMap,
    toLazyMap,
    fromList,
    toList,

    -- * Random access
    at, iat,

    -- * Range access
    from, to
) where

import Data.Series.Conversion (fromStrictMap, toStrictMap, fromLazyMap, toLazyMap, fromList, toList)
import Data.Series.Definition (Series(..))
import Data.Series.View       ( at, iat, from, to )
