module Data.Series.Windowing (
    windows,
    iwindows,
) where


import qualified Data.Set               as Set               
import           Data.Series.Definition ( Series(index) )
import           Data.Series.View       ( Range(..), select, slice )


windows :: Ord k 
        => (k -> Range k) 
        -> Series k a 
        -> [Series k a]
windows gen xs = [ xs `select` gen key | key <- Set.toAscList (index xs) ]


iwindows :: (Int -> Range Int) -> Series k a -> [Series k a]
iwindows gen xs = [ slice start end xs | MkRange start end <- gen <$> [0..length xs]]