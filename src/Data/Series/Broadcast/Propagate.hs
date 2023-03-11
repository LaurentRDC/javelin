{-# LANGUAGE FunctionalDependencies #-}

module Data.Series.Broadcast.Propagate (
    (+:), (-:), (*:), (/:),
) where

import           Data.Series.Broadcast  ( zipWith )
import           Data.Series.Definition ( Series(..) )
import           Prelude                hiding ( zipWith ) 


-- Same predecence as the usual Num operations
infixl 6 +:, -:
infixl 7 *:, /: 

class BroadcastNumMaybe a b c | a b -> c where
    (+:) :: a -> b -> c
    (-:) :: a -> b -> c
    (*:) :: a -> b -> c

class BroadcastFracMaybe a b c | a b -> c where
    (/:) :: a -> b -> c

-- All Series
instance (Ord k, Num a) => BroadcastNumMaybe (Series k a) (Series k a) (Series k (Maybe a)) where
    (+:) :: Series k a -> Series k a -> Series k (Maybe a)
    (+:) = zipWith (+)
    (-:) :: Series k a -> Series k a -> Series k (Maybe a)
    (-:) = zipWith (-)
    (*:) :: Series k a -> Series k a -> Series k (Maybe a)
    (*:) = zipWith (*)

instance (Ord k, Fractional a) => BroadcastFracMaybe (Series k a) (Series k a) (Series k (Maybe a)) where
    (/:) :: Series k a -> Series k a -> Series k (Maybe a)
    (/:) = zipWith (/)

-- Series on the left side
instance (Num a) => BroadcastNumMaybe (Series k a) a (Series k a) where
    (+:) :: Series k a -> a -> Series k a
    xs +: constant = fmap (+constant) xs
    (-:) :: Series k a -> a -> Series k a
    xs -: constant = fmap (\x -> x-constant) xs
    (*:) :: Series k a -> a -> Series k a
    xs *: constant = fmap (*constant) xs

instance Fractional a => BroadcastFracMaybe (Series k a) a (Series k a) where
    (/:) :: Series k a -> a -> Series k a
    xs /: constant = fmap (/constant) xs

-- Series on the right side
instance (Num a) => BroadcastNumMaybe a (Series k a) (Series k a) where
    (+:) :: a -> Series k a ->  Series k a
    constant +: xs = fmap (+constant) xs
    (-:) :: a -> Series k a ->  Series k a
    constant -: xs = fmap (constant -) xs
    (*:) :: a -> Series k a ->  Series k a
    constant *: xs = fmap (*constant) xs

instance Fractional a => BroadcastFracMaybe a (Series k a) (Series k a) where
    (/:) :: a -> Series k a -> Series k a
    constant /: xs = fmap (constant/) xs
