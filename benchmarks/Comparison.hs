-- This benchmarking script is forked from
-- https://github.com/haskell-perf/dictionaries/blob/master/Time.hs
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main (main) where

import           Control.DeepSeq  ( NFData, force )
import           Control.Monad    ( when )
import           Criterion.Main   ( defaultMainWith, defaultConfig, bench, bgroup, env, nf )
import           Criterion.Types  ( Config(csvFile) )
import           Data.List        ( foldl' )
import qualified Data.Map.Lazy
import qualified Data.Map.Strict
import           Data.Set         ( Set )
import qualified Data.Set         as Set 
import qualified Data.Series
import qualified Data.Vector
import qualified Data.Vector.Unboxed
import           System.Directory ( doesFileExist, removeFile )
import           System.Random    ( mkStdGen, Random(randoms) )

data Lookup =
  forall f. (NFData (f Int)) =>
            Lookup String
                   ([(Int, Int)] -> f Int)
                   (Int -> f Int ->  Maybe Int)

data Sum =
  forall f. (NFData (f Int)) =>
            Sum String ([(Int, Int)] -> f Int) (f Int -> Int)

data Mappend = 
  forall f. (NFData (f Int), Monoid (f Int)) =>
            Mappend String 
                   ([(Int, Int)] -> f Int)

data SliceByKeys =
  forall f. (NFData (f Int), Monoid (f Int)) =>
            SliceByKeys String 
                   ([(Int, Int)] -> f Int)
                   (Set Int -> f Int -> f Int)




main :: IO ()
main = do
  let fp = "out.csv"
  exists <- doesFileExist fp
  when exists (removeFile fp)
  defaultMainWith
    defaultConfig {csvFile = Just fp}
    [ bgroup
        "Lookup Int (Randomized)"
        (lookupRandomized
           [ Lookup "Data.Map.Lazy" Data.Map.Lazy.fromList Data.Map.Lazy.lookup
           , Lookup
               "Data.Map.Strict"
               Data.Map.Strict.fromList
               Data.Map.Strict.lookup
           , Lookup
               "Data.Series"
               Data.Series.fromList
               (flip Data.Series.at)
           , Lookup 
                "Data.Vector"
                (Data.Vector.fromList . map fst)
                (\ix -> Data.Vector.find (==ix))
           , Lookup 
                "Data.Vector.Unboxed"
                (Data.Vector.Unboxed.fromList . map fst)
                (\ix -> Data.Vector.Unboxed.find (==ix))
           ])
    , bgroup
        "Sum Int (Randomized)"
        (sumRandomized
           [ Sum "Data.Map.Lazy"   Data.Map.Lazy.fromList sum
           , Sum "Data.Map.Strict" Data.Map.Strict.fromList sum
           , Sum "Data.Series" Data.Series.fromList sum
           , Sum "Data.Vector" (Data.Vector.fromList . map fst) sum
           , Sum "Data.Vector.Unboxed" (Data.Vector.Unboxed.fromList . map fst) Data.Vector.Unboxed.sum
           ])
    , bgroup
      "Mappend Int (Randomized)"
      ( mappendRandomized 
          [ Mappend "Data.Map.Lazy" Data.Map.Lazy.fromList
          , Mappend "Data.Map.Strict" Data.Map.Strict.fromList
          , Mappend "Data.Series" Data.Series.fromList
          , Mappend "Data.Vector" (Data.Vector.fromList . map fst)
          , Mappend "Data.Vector.Unboxed" (Data.Vector.fromList . map fst)
          ])
    , bgroup
      "Slice by keys (Randomized)"
      ( sliceByKeyRandomized 
          [ SliceByKeys "Data.Map.Lazy" 
                        Data.Map.Lazy.fromList
                        (flip Data.Map.Lazy.restrictKeys)
          , SliceByKeys "Data.Map.Strict" 
                        Data.Map.Strict.fromList
                        (flip Data.Map.Strict.restrictKeys)
          , SliceByKeys "Data.Series" 
                        Data.Series.fromList
                        (flip Data.Series.select)
          ])
    ]

  where
    lookupRandomized funcs =
      [ env
        (let list = take i (zip (randoms (mkStdGen 0) :: [Int]) [1 ..])
             !elems = force (fromList list)
          in pure (list, elems))
        (\(~(list, elems)) ->
           bench (title ++ ":" ++ show i) $
           nf
             (foldl'
                  (\_ k ->
                     case func k elems of
                       Just !v -> v
                       Nothing -> 0)
                  0)
             (map fst list))
      | i <- [10, 100, 1000, 10000, 10000]
      , Lookup title fromList func <- funcs
      ]
    sumRandomized funcs =
      [ env
        (let list = take i (zip (randoms (mkStdGen 0) :: [Int]) [1 ..])
             !elems = force (fromList list)
          in pure (list, elems))
        (\(~(_, elems)) ->
           bench (title ++ ":" ++ show i) $
           nf func elems)
      | i <- [10, 100, 1000, 10000, 100000, 1000000]
      , Sum title fromList func <- funcs
      ]
    mappendRandomized funcs =
      [ env
        (let list1 = take i (zip (randoms (mkStdGen 0) :: [Int]) [1 ..])
             list2 = take i (zip (randoms (mkStdGen 0) :: [Int]) [1 ..])
             !elems1 = force (fromList list1)
             !elems2 = force (fromList list2)
          in pure (elems1, elems2))
        (\(~(elems1, elems2)) ->
           bench (title ++ ":" ++ show i) $
           nf mconcat [elems1, elems2])
      | i <- [10, 100, 1000, 10000, 100000, 1000000]
      , Mappend title fromList <- funcs
      ]
    sliceByKeyRandomized funcs = 
      [ env
        (let list = take i (zip (randoms (mkStdGen 0) :: [Int]) [1 ..])
             keys  = Set.fromList $ take (round ((fromIntegral i / 10) :: Double)) (randoms (mkStdGen 0) :: [Int])
             !elems = force (fromList list)
          in pure (keys, elems))
        (\(~(keys, elems)) ->
           bench (title ++ ":" ++ show i) $
           nf (slice keys) elems)
      | i <- [10, 100, 1000, 10000, 100000, 1000000]
      , SliceByKeys title fromList slice <- funcs
      ]