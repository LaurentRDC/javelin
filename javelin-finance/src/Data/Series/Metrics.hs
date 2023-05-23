
module Data.Series.Metrics ( sharpeRatio ) where

import           Data.Vector.Generic ( Vector )
import           Data.Series.Generic ( Series )
import qualified Data.Series.Generic as GSeries


-- $setup
-- >>> import qualified Data.Series as Series


-- | \(O(n)\) Calculate the Sharpe ratio of excess returns. 
--
-- >>> let returns = Series.fromList $ zip [(0::Int)..] [ (1.0::Double),2.0,3.0,2.0 ]
-- >>> returns
-- index | values
-- ----- | ------
--     0 |    1.0
--     1 |    2.0
--     2 |    3.0
--     3 |    2.0
-- >>> sharpeRatio returns
-- 2.82842712474619
sharpeRatio :: ( RealFloat a
               , Vector v a
               , Vector v (Int, a, a)
               ) => Series v k a -> a
sharpeRatio returns = GSeries.mean returns 
                    / GSeries.std returns