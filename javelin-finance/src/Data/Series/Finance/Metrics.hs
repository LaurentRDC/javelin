
module Data.Series.Finance.Metrics ( 
    sharpeRatio,

    -- * Drawdowns
    DrawDown, amount, duration, recovery,
    drawdowns, maxDrawDown,
) where

import qualified Data.List.NonEmpty as NE
import           Data.Vector.Generic ( Vector )
import           Data.Series.Generic ( Series, select, index )
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


-- | Representation of a drawdown: a continuous series of non-positive returns.
data DrawDown v k a 
    = MkDrawDown { getDrawDown :: Series v k a
                 , getRecovery :: Series v k a 
                 }


-- | Determine the amount of a drawdown.
amount :: (Vector v a, Num a) => DrawDown v k a -> a
amount = GSeries.sum . getDrawDown


-- | Determine the length of a drawdown.
--
-- In order to minimize constraints on the key type,
-- the duration is returned as a tuple of the starting key and ending key.
duration :: Ord k => DrawDown v k a -> (k, k)
duration dd = let ix = GSeries.index $ getDrawDown dd
               in (minimum ix, maximum ix)


-- | Determine the length of the recovery from a drawdown.
--
-- In order to minimize constraints on the key type,
-- the duration is returned as a tuple of the starting key and ending key.
recovery :: Ord k => DrawDown v k a -> (k, k)
recovery dd = let ix = GSeries.index $ getRecovery dd
               in (minimum ix, maximum ix)


-- | Compute a list of drawdowns from a series of returns.
drawdowns :: (Vector v a, Num a, Ord a, Ord k) 
          => Series v k a 
          -> [DrawDown v k a]
drawdowns xs = go xs mempty
    where
        go ys acc
            | GSeries.length ys == 0 = acc
            | otherwise = let ys' = GSeries.dropWhile (>0) ys
                              (dd, after) = part (<=0) ys'
                              toRecover = -1 * GSeries.sum dd
                              recov = index $ GSeries.takeWhile (<toRecover) $ GSeries.postscanl (+) 0 after
                           in go after (acc <> [MkDrawDown dd (after `select` recov)])

        part f s = (GSeries.takeWhile f s, GSeries.dropWhile f s)


-- | Returns the most negative drawdown from a series of returns.
maxDrawDown :: (Vector v a, Num a, Ord a, Ord k) 
            => Series v k a 
            -> a
maxDrawDown xs = maybe 0 (minimum . fmap amount) 
               $ NE.nonEmpty (drawdowns xs)