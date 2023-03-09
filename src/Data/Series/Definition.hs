module Data.Series.Definition ( 
    Series(..),
) where


import           Data.Bifoldable ( Bifoldable(..) )
import           Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import           Data.Vector     ( Vector )
import qualified Data.Vector     as Vector


-- | A series is a contiguous array of values of type @a@,
-- indexed by keys of type @k@.
data Series k a 
    = MkSeries { index  :: !(Map k Int)
               , values :: !(Vector a)
               }
    deriving (Show)

instance (Eq k, Eq a) => Eq (Series k a) where
    (==) :: Series k a -> Series k a -> Bool
    (MkSeries ks1 vs1) == (MkSeries ks2 vs2) = (ks1 == ks2) && (vs1 == vs2)

instance Functor (Series k) where
    fmap :: (a -> b) -> Series k a -> Series k b
    fmap f (MkSeries ks vs) = MkSeries ks (Vector.map f vs)

instance Foldable (Series k) where
    foldMap :: Monoid m => (a -> m) -> Series k a -> m
    foldMap f (MkSeries _ vs) = foldMap f vs

instance Bifoldable Series where
    bifoldMap :: Monoid m => (k -> m) -> (a -> m) -> Series k a -> m
    bifoldMap fk fv (MkSeries ks vs) = Map.foldMapWithKey (\k _ -> fk k) ks <> foldMap fv vs
