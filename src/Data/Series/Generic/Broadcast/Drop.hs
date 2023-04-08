{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}

module Data.Series.Generic.Broadcast.Drop ( 
    (+|), (-|), (*|), (/|), (==|), (/=|)
) where

import           Data.Series.Generic.Definition ( Series )
import qualified Data.Series.Generic.Definition as Series
import           Data.Series.Generic.Broadcast  ( zipWithMatched )
import           Data.Vector.Generic    ( Vector )


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
instance (Vector v a, Ord k, Num a) => BroadcastNumMatched (Series v k a) (Series v k a) (Series v k a) where
    (+|) :: Series v k a -> Series v k a -> Series v k a
    (+|) = zipWithMatched (+)
    (-|) :: Series v k a -> Series v k a -> Series v k a
    (-|) = zipWithMatched (-)
    (*|) :: Series v k a -> Series v k a -> Series v k a
    (*|) = zipWithMatched (*)

instance (Vector v a, Ord k, Fractional a) => BroadcastFracMatched (Series v k a) (Series v k a) (Series v k a) where
    (/|) :: Series v k a -> Series v k a -> Series v k a
    (/|) = zipWithMatched (/)

instance (Vector v a, Vector v Bool, Ord k, Eq a) => BroadcastEqMatched (Series v k a) (Series v k a) (Series v k Bool) where
    (==|) :: Series v k a -> Series v k a -> Series v k Bool
    (==|) = zipWithMatched (==)

    (/=|) :: Series v k a -> Series v k a -> Series v k Bool
    (/=|) = zipWithMatched (/=)

-- Series on the left side
instance (Vector v a, Num a) => BroadcastNumMatched (Series v k a) a (Series v k a) where
    (+|) :: Series v k a -> a -> Series v k a
    xs +| constant = Series.map (+constant) xs
    (-|) :: Series v k a -> a -> Series v k a
    xs -| constant = Series.map (\x -> x-constant) xs
    (*|) :: Series v k a -> a -> Series v k a
    xs *| constant = Series.map (*constant) xs

instance (Vector v a, Fractional a) => BroadcastFracMatched (Series v k a) a (Series v k a) where
    (/|) :: Series v k a -> a -> Series v k a
    xs /| constant = Series.map (/constant) xs

instance (Vector v a, Vector v Bool, Eq a) => BroadcastEqMatched (Series v k a) a (Series v k Bool) where
    (==|) :: Series v k a -> a -> Series v k Bool
    xs ==| x = Series.map (==x) xs

    (/=|) :: Series v k a -> a -> Series v k Bool
    xs /=| x = Series.map (/=x) xs

-- Series on the right side
instance (Vector v a, Num a) => BroadcastNumMatched a (Series v k a) (Series v k a) where
    (+|) :: a -> Series v k a ->  Series v k a
    constant +| xs = Series.map (+constant) xs
    (-|) :: a -> Series v k a ->  Series v k a
    constant -| xs = Series.map (constant -) xs
    (*|) :: a -> Series v k a ->  Series v k a
    constant *| xs = Series.map (*constant) xs

instance (Vector v a, Fractional a) => BroadcastFracMatched a (Series v k a)  (Series v k a) where
    (/|) :: a -> Series v k a -> Series v k a
    constant /| xs = Series.map (constant/) xs

instance (Vector v a, Vector v Bool, Eq a) => BroadcastEqMatched a (Series v k a)  (Series v k Bool) where
    (==|) :: a -> Series v k a -> Series v k Bool
    x ==| xs = Series.map (==x) xs

    (/=|) :: a -> Series v k a -> Series v k Bool
    x /=| xs = Series.map (/=x) xs