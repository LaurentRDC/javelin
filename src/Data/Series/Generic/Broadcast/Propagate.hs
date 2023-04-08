{-# LANGUAGE FunctionalDependencies #-}

module Data.Series.Generic.Broadcast.Propagate (
    (+:), (-:), (*:), (/:), (==:), (/=:),
) where

import           Data.Series.Generic.Broadcast  ( zipWith )
import           Data.Series.Generic.Definition ( Series(..) )
import qualified Data.Series.Generic.Definition as Series
import           Data.Vector.Generic    ( Vector )
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

class BroadcastEqMaybe a b c | a b -> c where
    (==:) :: a -> b -> c
    (/=:) :: a -> b -> c

-- All Series
instance (Vector v a, Vector v (Maybe a), Ord k, Num a) => BroadcastNumMaybe (Series v k a) (Series v k a) (Series v k (Maybe a)) where
    (+:) :: Series v k a -> Series v k a -> Series v k (Maybe a)
    (+:) = zipWith (+)
    (-:) :: Series v k a -> Series v k a -> Series v k (Maybe a)
    (-:) = zipWith (-)
    (*:) :: Series v k a -> Series v k a -> Series v k (Maybe a)
    (*:) = zipWith (*)

instance (Vector v a, Vector v (Maybe a), Ord k, Fractional a) => BroadcastFracMaybe (Series v k a) (Series v k a) (Series v k (Maybe a)) where
    (/:) :: Series v k a -> Series v k a -> Series v k (Maybe a)
    (/:) = zipWith (/)

instance (Vector v a, Vector v Bool, Vector v (Maybe Bool), Ord k, Eq a) => BroadcastEqMaybe (Series v k a) (Series v k a) (Series v k (Maybe Bool)) where
    (==:) :: Series v k a -> Series v k a -> Series v k (Maybe Bool)
    (==:) = zipWith (==)

    (/=:) :: Series v k a -> Series v k a -> Series v k (Maybe Bool)
    (/=:) = zipWith (/=)

-- Series on the left side
instance (Vector v a, Num a) => BroadcastNumMaybe (Series v k a) a (Series v k a) where
    (+:) :: Series v k a -> a -> Series v k a
    xs +: constant = Series.map (+constant) xs
    (-:) :: Series v k a -> a -> Series v k a
    xs -: constant = Series.map (\x -> x-constant) xs
    (*:) :: Series v k a -> a -> Series v k a
    xs *: constant = Series.map (*constant) xs

instance (Vector v a, Fractional a) => BroadcastFracMaybe (Series v k a) a (Series v k a) where
    (/:) :: Series v k a -> a -> Series v k a
    xs /: constant = Series.map (/constant) xs

instance (Vector v a, Vector v Bool, Eq a) => BroadcastEqMaybe (Series v k a) a (Series v k Bool) where
    (==:) :: Series v k a -> a -> Series v k Bool
    xs ==: x = Series.map (==x) xs

    (/=:) :: Series v k a -> a -> Series v k Bool
    xs /=: x = Series.map (/=x) xs

-- Series on the right side
instance (Vector v a, Num a) => BroadcastNumMaybe a (Series v k a) (Series v k a) where
    (+:) :: a -> Series v k a ->  Series v k a
    constant +: xs = Series.map (+constant) xs
    (-:) :: a -> Series v k a ->  Series v k a
    constant -: xs = Series.map (constant -) xs
    (*:) :: a -> Series v k a ->  Series v k a
    constant *: xs = Series.map (*constant) xs

instance (Vector v a, Fractional a) => BroadcastFracMaybe a (Series v k a) (Series v k a) where
    (/:) :: a -> Series v k a -> Series v k a
    constant /: xs = Series.map (constant/) xs

instance (Vector v a, Vector v Bool, Eq a) => BroadcastEqMaybe a (Series v k a)  (Series v k Bool) where
    (==:) :: a -> Series v k a -> Series v k Bool
    x ==: xs = Series.map (==x) xs

    (/=:) :: a -> Series v k a -> Series v k Bool
    x /=: xs = Series.map (/=x) xs