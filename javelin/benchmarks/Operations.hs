
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
    let srs1        = Series.fromList $ zip [0..] [1::Int .. 2^(12::Int)]
        srs2        = Series.fromList $ zip [0,2..] [1::Int .. 2^(12::Int)]
        elems      = Index.toSet $ Series.index srs1
        small      = Set.fromAscList  [1::Int ..  2^(8::Int)]
        elems_even = Set.fromDistinctAscList  [2::Int, 4..2^(12::Int)]
        elems_odd  = Set.fromDistinctAscList  [1::Int, 3..2^(12::Int)]
    evaluate $ rnf [elems, small, elems_even, elems_odd]
    evaluate $ rnf [srs1, srs2]
    defaultMain
        [ bench "at" $ whnf (at elems_even) srs1
        , bench "iat" $ whnf (iat elems_even) srs1
        , bench "select" $ whnf (select elems_odd) srs1
        , bench "mappend" $ whnf (mappend' srs1) srs2
        , bench "zipWithMatched" $ whnf (zipWithMatched srs1) srs1
        , bench "group by ... aggregate with ..." $ whnf (groupbyagg small) srs1
        , bench "group by ... fold with ..." $ whnf (groupbyfold small) srs1
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


mappend' :: Series Int Int -> Series Int Int -> Int
mappend' xs ys = sum $ xs <> ys


zipWithMatched :: Series Int Int -> Series Int Int -> Int
zipWithMatched xs ys = length $ Series.zipWithMatched (+) xs ys


groupbyagg :: Set Int -> Series Int Int -> Int
groupbyagg ks s = foldl' go 0 ks
    where
        go n k = n + product (s `Series.groupBy` (`mod` (k + 1)) `Series.aggregateWith` sum)

groupbyfold :: Set Int -> Series Int Int -> Int
groupbyfold ks s = foldl' go 0 ks
    where
        go n k = n + product (s `Series.groupBy` (`mod` (k + 1)) `Series.foldWith` (+))