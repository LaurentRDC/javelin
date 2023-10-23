
module Data.Series.Generic.Scans (
    postscanl,

    -- * Filling missing data
    forwardFill,
) where

import           Data.Series.Generic.Definition ( Series(..) )

import           Data.Vector.Generic            ( Vector )
import qualified Data.Vector.Generic            as Vector    

-- $setup
-- >>> import qualified Data.Series.Generic ( Series )
-- >>> import qualified Data.Series.Generic as Series
-- >>> import qualified Data.Series.Index as Index

-- | /O(n)/ Left-to-right postscan.
--
-- >>> import qualified Data.Vector as V 
-- >>> let xs = Series.fromList (zip [0..] [1,2,3,4]) :: Series V.Vector Int Int
-- >>> xs
-- index | values
-- ----- | ------
--     0 |      1
--     1 |      2
--     2 |      3
--     3 |      4
-- >>> postscanl (+) 0 xs
-- index | values
-- ----- | ------
--     0 |      1
--     1 |      3
--     2 |      6
--     3 |     10
postscanl :: (Vector v a, Vector v b) => (a -> b -> a) -> a -> Series v k b -> Series v k a
{-# INLINE postscanl #-}
postscanl f s (MkSeries ix vs) = MkSeries ix $ Vector.postscanl f s vs


-- | /O(n)/ Replace all instances of 'Nothing' with the last previous
-- value which was not 'Nothing'.
--
-- >>> import qualified Data.Vector as V 
-- >>> let xs = Series.fromList (zip [0..] [Just 1, Just 2,Nothing, Just 3]) :: Series V.Vector Int (Maybe Int)
-- >>> xs
-- index |  values
-- ----- |  ------
--     0 |  Just 1
--     1 |  Just 2
--     2 | Nothing
--     3 |  Just 3
-- >>> forwardFill 0 xs
-- index | values
-- ----- | ------
--     0 |      1
--     1 |      2
--     2 |      2
--     3 |      3
--
-- If the first entry of the series is missing, the first input to 'forwardFill' will be used:
--
-- >>> let ys = Series.fromList (zip [0..] [Nothing, Just 2,Nothing, Just 3]) :: Series V.Vector Int (Maybe Int)
-- >>> ys
-- index |  values
-- ----- |  ------
--     0 | Nothing
--     1 |  Just 2
--     2 | Nothing
--     3 |  Just 3
-- >>> forwardFill 0 ys
-- index | values
-- ----- | ------
--     0 |      0
--     1 |      2
--     2 |      2
--     3 |      3
forwardFill :: (Vector v a, Vector v (Maybe a))
            => a -- ^ Until the first non-'Nothing' is found, 'Nothing' will be filled with this value.
            -> Series v k (Maybe a)
            -> Series v k a
{-# INLINE forwardFill #-}
forwardFill = postscanl go
    where
        go :: a -> Maybe a -> a
        go lastValid Nothing = lastValid
        go _        (Just v) = v
