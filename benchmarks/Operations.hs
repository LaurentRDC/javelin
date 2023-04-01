
import           Control.DeepSeq    ( rnf )
import           Control.Exception  ( evaluate )
import           Criterion.Main     ( bench, whnf, defaultMain )

import           Data.Foldable      ( Foldable(foldl') )
import           Data.Set           ( Set )     
import qualified Data.Set           as Set
import           Data.Series        ( Series )
import qualified Data.Series        as Series
import qualified Data.Series.Index  as Index


main :: IO ()
main = do
    let srs        = Series.fromList $ zip [0..] [1::Int .. 2^(12::Int)]
        elems      = Index.toSet $ Series.index srs
        small      = Set.fromAscList  [1::Int ..  2^(8::Int)]
        elems_even = Set.fromDistinctAscList  [2::Int, 4..2^(12::Int)]
        elems_odd  = Set.fromDistinctAscList  [1::Int, 3..2^(12::Int)]
    evaluate $ rnf [elems, small, elems_even, elems_odd]
    evaluate $ rnf [srs]
    defaultMain
        [ bench "at" $ whnf (at elems_even) srs
        , bench "iat" $ whnf (iat elems_even) srs
        , bench "select" $ whnf (select elems_odd) srs
        , bench "group by ... aggregate with ..." $ whnf (groupby small) srs
        ]

at :: Set Int -> Series Int Int -> Int
at xs s = foldl' go 0 xs
    where
        go n x = case s `Series.at` x of 
            Just _ -> n + 1
            Nothing -> n

iat :: Set Int -> Series Int Int -> Int
iat xs s = foldl' go 0 xs
    where
        go n x = case s `Series.iat` x of 
            Just _ -> n + 1
            Nothing -> n
  
select :: Set Int -> Series Int Int -> Int
select ks s = foldl' go 0 ks
    where
        go n k = n + length (s `Series.select` ((k-100) `Series.to` (k+100)))

groupby :: Set Int -> Series Int Int -> Int
groupby ks s = foldl' go 0 ks
    where
        go n k = n + product (s `Series.groupBy` (`mod` (k + 1)) `Series.aggregateWith` sum)