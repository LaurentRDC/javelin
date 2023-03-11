
module Data.Series.Broadcast (
    zipWith, zipWithMatched,
    -- * Broadcastable operations that may leave holes
    (+:), (-:), (*:),
    -- * Broadcastable operations only on matched keys
    (+|), (-|), (*|)
) where

import           Data.Series.Definition ( Series(MkSeries, index) )
import           Data.Series.View       ( select )
import qualified Data.Set               as Set
import qualified Data.Vector            as Vector
import           Prelude                hiding ( zipWith, (<*), (*>), (<*>) ) 


-- | Apply a function elementwise to two series, matching elements
-- based on their keys. For keys present only in the left or right series, 
-- the value @Nothing@ is returned.
zipWith :: Ord k => (a -> b -> c) -> Series k a -> Series k b -> Series k (Maybe c)
zipWith f left right
    = let matched = zipWithMatched f left right
          matchedKeys   = index matched
          allKeys       = index left `Set.union` index right
          unmatchedKeys = allKeys `Set.difference` matchedKeys
          unmatched     = MkSeries unmatchedKeys (Vector.replicate (Set.size unmatchedKeys) Nothing)
       in (Just <$> matched) <> unmatched
{-# INLINE zipWith #-}


-- | Apply a function elementwise to two series, matching elements
-- based on their keys. Keys present only in the left or right series are dropped.
zipWithMatched :: Ord k => (a -> b -> c) -> Series k a -> Series k b -> Series k c
zipWithMatched f left right
    = let matchedKeys   = index left `Set.intersection` index right

          (MkSeries _ xs) = left  `select` matchedKeys
          (MkSeries _ ys) = right `select` matchedKeys
          -- The following construction relies on the fact that keys are always sorted
          matched         = MkSeries matchedKeys $ Vector.zipWith f xs ys
       in matched
{-# INLINE zipWithMatched #-}


-- Same predecence as the usual Num operations
infixl 6 +:, +|, -:, -|
infixl 7 *:, *| 

class BroadcastNumMaybe a b c where
    (+:) :: a -> b -> c
    (-:) :: a -> b -> c
    (*:) :: a -> b -> c

class BroadcastNumMatched a b c where
    (+|) :: a -> b -> c
    (-|) :: a -> b -> c
    (*|) :: a -> b -> c

-- -- All Nums
-- instance Num a => BroadcastNumMaybe a a a where
--     (+:) :: a -> a -> a
--     (+:) = (+)
--     (-:) :: a -> a -> a
--     (-:) = (-)
--     (*:) :: a -> a -> a
--     (*:) = (*)

-- instance Num a => BroadcastNumMatched a a a where
--     (+|) :: a -> a -> a
--     (+|) = (+)
--     (-|) :: a -> a -> a
--     (-|) = (-)
--     (*|) :: a -> a -> a
--     (*|) = (*)

-- All Series
instance (Ord k, Num a) => BroadcastNumMaybe (Series k a) (Series k a) (Series k (Maybe a)) where
    (+:) :: Series k a -> Series k a -> Series k (Maybe a)
    (+:) = zipWith (+)
    (-:) :: Series k a -> Series k a -> Series k (Maybe a)
    (-:) = zipWith (-)
    (*:) :: Series k a -> Series k a -> Series k (Maybe a)
    (*:) = zipWith (*)

instance (Ord k, Num a) => BroadcastNumMatched (Series k a) (Series k a) (Series k a) where
    (+|) :: Series k a -> Series k a -> Series k a
    (+|) = zipWithMatched (+)
    (-|) :: Series k a -> Series k a -> Series k a
    (-|) = zipWithMatched (-)
    (*|) :: Series k a -> Series k a -> Series k a
    (*|) = zipWithMatched (*)

-- Series on the left side
instance (Num a) => BroadcastNumMaybe (Series k a) a (Series k a) where
    (+:) :: Series k a -> a -> Series k a
    xs +: constant = fmap (+constant) xs
    (-:) :: Series k a -> a -> Series k a
    xs -: constant = fmap (\x -> x-constant) xs
    (*:) :: Series k a -> a -> Series k a
    xs *: constant = fmap (*constant) xs

instance (Num a) => BroadcastNumMatched (Series k a) a (Series k a) where
    (+|) :: Series k a -> a -> Series k a
    xs +| constant = fmap (+constant) xs
    (-|) :: Series k a -> a -> Series k a
    xs -| constant = fmap (\x -> x-constant) xs
    (*|) :: Series k a -> a -> Series k a
    xs *| constant = fmap (*constant) xs

-- Series on the right side
instance (Num a) => BroadcastNumMaybe a (Series k a) (Series k a) where
    (+:) :: a -> Series k a ->  Series k a
    constant +: xs = fmap (+constant) xs
    (-:) :: a -> Series k a ->  Series k a
    constant -: xs = fmap (constant -) xs
    (*:) :: a -> Series k a ->  Series k a
    constant *: xs = fmap (*constant) xs

instance (Num a) => BroadcastNumMatched a (Series k a) (Series k a) where
    (+|) :: a -> Series k a ->  Series k a
    constant +| xs = fmap (+constant) xs
    (-|) :: a -> Series k a ->  Series k a
    constant -| xs = fmap (constant -) xs
    (*|) :: a -> Series k a ->  Series k a
    constant *| xs = fmap (*constant) xs