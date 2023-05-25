
module Data.Series.Generic.Scans (postscanl) where

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
postscanl f s (MkSeries ix vs) = MkSeries ix $ Vector.postscanl f s vs
