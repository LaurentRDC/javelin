{-# LANGUAGE RecordWildCards #-}
-- Having strict accumulators is key to keeping a good
-- memory footprint, given that all metrics need to be computed
-- in a single pass.
{-# LANGUAGE StrictData      #-}
module Data.Series.Finance.Metrics (
    -- * Financial metrics
    sharpeRatio,
    sortinoRatio,
    maxDrawDown,
) where

import           Control.Foldl       ( Fold(..) )
import qualified Control.Foldl       as Fold


-- $setup
-- >>> import qualified Data.Series as Series


-- | \(O(n)\) Sharpe ratio of excess returns. 
--
-- >>> let returns = Series.fromList $ zip [(0::Int)..] [ (1.0::Double), 2.0, 3.0, 2.0 ]
-- >>> returns
-- index | values
-- ----- | ------
--     0 |    1.0
--     1 |    2.0
--     2 |    3.0
--     3 |    2.0
-- >>> Series.fold sharpeRatio returns
-- 2.82842712474619
sharpeRatio :: RealFloat a => Fold a a
-- Note that the standard deviation of returns and standard deviation of excess returns
-- are always equal; the standard deviation is invariant under addition of a constant.
sharpeRatio = (/) <$> Fold.mean <*> Fold.std
{-# INLINABLE sharpeRatio #-}


-- | \(O(n)\) Sortino ratio of excess returns.
--
-- There are two edge cases. If the input series is empty, or there are no negative returns, the result is @NaN@;
--
-- >>> let returns = Series.fromList $ zip [(0::Int)..] [ (1.0::Double), -2.0, 3.0, -1.0 ]
-- >>> returns
-- index | values
-- ----- | ------
--     0 |    1.0
--     1 |   -2.0
--     2 |    3.0
--     3 |   -1.0
-- >>> Series.fold sortinoRatio returns
-- 0.5000000000000001
sortinoRatio :: RealFloat a => Fold a a
-- Note that the standard deviation of returns and standard deviation of excess returns
-- are always equal; the standard deviation is invariant under addition of a constant.
sortinoRatio = (/) <$> Fold.mean <*> Fold.prefilter (<0) Fold.std
{-# INLINABLE sortinoRatio #-}


-- | Returns the most negative drawdown from a series of returns.
--
-- >>> let returns = Series.fromList $ zip [(0::Int)..] [ (-1.0::Double), -2.0, 0.0, -3.0, 1.0, -5.0 ]
-- >>> returns
-- index | values
-- ----- | ------
--     0 |   -1.0
--     1 |   -2.0
--     2 |    0.0
--     3 |   -3.0
--     4 |    1.0
--     5 |   -5.0
-- >>> Series.fold maxDrawDown returns
-- -10.0
maxDrawDown :: (Ord a, Num a) => Fold a a
maxDrawDown = Fold advance (MkDrawDownState 0 0 0) cumulativeMaxDraw
    where
        advance MkDrawDownState{..} ret 
            = let newAccumulatedReturn = ret + cumulativeReturn
                  draw = newAccumulatedReturn - cumulativeMax
               in MkDrawDownState{ cumulativeMax     = max cumulativeMax newAccumulatedReturn
                                 , cumulativeReturn  = newAccumulatedReturn
                                 , cumulativeMaxDraw = min cumulativeMaxDraw draw
                                 }
{-# INLINABLE maxDrawDown #-}

data DrawDownState a = MkDrawDownState { cumulativeMax     :: !a 
                                       , cumulativeReturn  :: !a
                                       , cumulativeMaxDraw :: !a
                                       }