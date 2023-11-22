{-# LANGUAGE RecordWildCards #-}
-- Having strict accumulators is key to keeping a good
-- memory footprint, given that all metrics need to be computed
-- in a single pass.
{-# LANGUAGE StrictData      #-}
module Data.Series.Finance.Metrics (
    Metric, compute,

    -- * Basic financial metrics
    sharpeRatio,
    sortinoRatio,
    maxDrawDown,
) where

import           Control.Foldl       ( Fold(..) )
import qualified Control.Foldl       as Fold
import           Data.Series.Generic ( Series )
import qualified Data.Series         as Boxed
import           Data.Vector.Generic ( Vector )


-- $setup
-- >>> import qualified Data.Series as Series

-- | Type synonym for metrics.
-- Note that through underlying 'Fold' mechanism from "Control.Foldl", a 'Metric' always traverses
-- a series once. 
type Metric k a b = Fold (k, a) b


-- | Compute a given metric over a series.
-- Note that through underlying 'Fold' mechanism, a 'Metric' always traverses
-- a series once. Therefore, it is better to combine multiple metrics before calling 'compute':
--
-- >>> let returns = Series.fromList $ zip [(0::Int)..] [ (1.0::Double), -2.0, 3.0, -1.0 ]
-- >>> returns
-- index | values
-- ----- | ------
--     0 |    1.0
--     1 |   -2.0
--     2 |    3.0
--     3 |   -1.0
-- >>> let allMyMetrics = (,,) <$> maxDrawDown <*> sharpeRatio <*> sortinoRatio
-- >>> returns `compute` allMyMetrics
-- (-2.0,0.1301889109808239,0.5000000000000001)
compute :: Vector v a 
        => Series v k a 
        -> Metric k a b 
        -> b
compute xs metric 
    = Fold.fold metric $ Boxed.toVector $ toBoxedSeries xs
{-# INLINE compute #-}
    

-- Since unboxed vectors are not foldable, unboxed series are also not foldable.
-- I don't have a great idea on how to define metrics which can be applied to
-- boxed and unboxed series, so we resort to converting all unboxed series
-- to boxed series
toBoxedSeries :: Vector v a => Series v k a -> Boxed.Series k a
toBoxedSeries = Boxed.convert
{-# INLINE toBoxedSeries #-}


-- | Upgrade a 'Fold' to a 'Metric', by ignoring keys in a 'Series'.
ignoringKeys :: Fold a b -> Metric k a b
ignoringKeys = Fold.premap snd
{-# INLINE ignoringKeys #-}


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
-- >>> returns `compute` sharpeRatio
-- 2.82842712474619
sharpeRatio :: RealFloat a => Metric k a a
-- Note that the standard deviation of returns and standard deviation of excess returns
-- are always equal; the standard deviation is invariant under addition of a constant.
sharpeRatio = ignoringKeys $ (/) <$> Fold.mean <*> Fold.std
{-# INLINE sharpeRatio #-}


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
-- >>> returns `compute` sortinoRatio
-- 0.5000000000000001
sortinoRatio :: RealFloat a => Metric k a a
-- Note that the standard deviation of returns and standard deviation of excess returns
-- are always equal; the standard deviation is invariant under addition of a constant.
sortinoRatio = ignoringKeys $ (/) <$> Fold.mean <*> Fold.prefilter (<0) Fold.std
{-# INLINE sortinoRatio #-}


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
-- >>> returns `compute` maxDrawDown
-- -10.0
maxDrawDown :: (Ord a, Num a) => Metric k a a
maxDrawDown = ignoringKeys $ Fold advance (MkDrawDownState 0 0 0) cumulativeMaxDraw
    where
        advance MkDrawDownState{..} ret 
            = let newAccumulatedReturn = ret + cumulativeReturn
                  draw = newAccumulatedReturn - cumulativeMax
               in MkDrawDownState{ cumulativeMax     = max cumulativeMax newAccumulatedReturn
                                 , cumulativeReturn  = newAccumulatedReturn
                                 , cumulativeMaxDraw = min cumulativeMaxDraw draw
                                 }
{-# INLINE maxDrawDown #-}

data DrawDownState a = MkDrawDownState { cumulativeMax     :: !a 
                                       , cumulativeReturn  :: !a
                                       , cumulativeMaxDraw :: !a
                                       }