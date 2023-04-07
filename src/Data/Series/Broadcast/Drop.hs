{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}

module Data.Series.Broadcast.Drop ( 
    (+|), (-|), (*|), (/|), (==|), (/=|)
) where

import Data.Series.Definition ( Series )
import Data.Series.Broadcast  ( zipWithMatched )


infixl 6 +|, -|
infixl 7 *|, /| 
infix 4 ==|, /=|


class BroadcastNumMatched a b c | a b -> c where
    (+|) :: a -> b -> c
    (-|) :: a -> b -> c
    (*|) :: a -> b -> c

class BroadcastFracMatched a b c | a b -> c where
    (/|) :: a -> b -> c

class BroadcastEqMatched a b c | a b -> c where
    (==|) :: a -> b -> c
    (/=|) :: a -> b -> c

-- All series
instance (Ord k, Num a) => BroadcastNumMatched (Series k a) (Series k a) (Series k a) where
    (+|) :: Series k a -> Series k a -> Series k a
    (+|) = zipWithMatched (+)
    (-|) :: Series k a -> Series k a -> Series k a
    (-|) = zipWithMatched (-)
    (*|) :: Series k a -> Series k a -> Series k a
    (*|) = zipWithMatched (*)

instance (Ord k, Fractional a) => BroadcastFracMatched (Series k a) (Series k a) (Series k a) where
    (/|) :: Series k a -> Series k a -> Series k a
    (/|) = zipWithMatched (/)

instance (Ord k, Eq a) => BroadcastEqMatched (Series k a) (Series k a) (Series k Bool) where
    (==|) :: Series k a -> Series k a -> Series k Bool
    (==|) = zipWithMatched (==)

    (/=|) :: Series k a -> Series k a -> Series k Bool
    (/=|) = zipWithMatched (/=)

-- Series on the left side
instance (Num a) => BroadcastNumMatched (Series k a) a (Series k a) where
    (+|) :: Series k a -> a -> Series k a
    xs +| constant = fmap (+constant) xs
    (-|) :: Series k a -> a -> Series k a
    xs -| constant = fmap (\x -> x-constant) xs
    (*|) :: Series k a -> a -> Series k a
    xs *| constant = fmap (*constant) xs

instance Fractional a => BroadcastFracMatched (Series k a) a (Series k a) where
    (/|) :: Series k a -> a -> Series k a
    xs /| constant = fmap (/constant) xs

instance Eq a => BroadcastEqMatched (Series k a) a (Series k Bool) where
    (==|) :: Series k a -> a -> Series k Bool
    xs ==| x = fmap (==x) xs

    (/=|) :: Series k a -> a -> Series k Bool
    xs /=| x = fmap (/=x) xs

-- Series on the right side
instance (Num a) => BroadcastNumMatched a (Series k a) (Series k a) where
    (+|) :: a -> Series k a ->  Series k a
    constant +| xs = fmap (+constant) xs
    (-|) :: a -> Series k a ->  Series k a
    constant -| xs = fmap (constant -) xs
    (*|) :: a -> Series k a ->  Series k a
    constant *| xs = fmap (*constant) xs

instance Fractional a => BroadcastFracMatched a (Series k a)  (Series k a) where
    (/|) :: a -> Series k a -> Series k a
    constant /| xs = fmap (constant/) xs

instance Eq a => BroadcastEqMatched a (Series k a)  (Series k Bool) where
    (==|) :: a -> Series k a -> Series k Bool
    x ==| xs = fmap (==x) xs

    (/=|) :: a -> Series k a -> Series k Bool
    x /=| xs = fmap (/=x) xs