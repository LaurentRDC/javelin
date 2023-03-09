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
import qualified Data.HashMap.Lazy
import qualified Data.HashMap.Strict
import           Data.List        ( foldl' )
import qualified Data.Map.Lazy
import qualified Data.Map.Strict
import qualified Data.Series
import qualified Data.Vector
import           System.Directory ( doesFileExist, removeFile )
import           System.Random    ( mkStdGen, Random(randoms) )

data Lookup =
  forall f. (NFData (f Int)) =>
            Lookup String
                   ([(Int, Int)] -> f Int)
                   (Int -> f Int ->  Maybe Int)


data Sum =
  forall f. (NFData (f Int), Foldable f) =>
            Sum String ([(Int, Int)] -> f Int)


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
               "Data.HashMap.Lazy"
               Data.HashMap.Lazy.fromList
               Data.HashMap.Lazy.lookup
           , Lookup
               "Data.HashMap.Strict"
               Data.HashMap.Strict.fromList
               Data.HashMap.Strict.lookup
           , Lookup
               "Data.Series"
               Data.Series.fromList
               (flip Data.Series.at)
           , Lookup 
                "Data.Vector"
                (Data.Vector.fromList . map fst)
                (\ix -> Data.Vector.find (==ix))

           ])
    , bgroup
        "Sum Int (Randomized)"
        (sumRandomized
           [ Sum "Data.Map.Lazy"   Data.Map.Lazy.fromList
           , Sum "Data.Map.Strict" Data.Map.Strict.fromList
           , Sum "Data.HashMap.Lazy" Data.HashMap.Lazy.fromList
           , Sum "Data.HashMap.Strict" Data.HashMap.Strict.fromList
           , Sum "Data.Series" Data.Series.fromList
           , Sum "Data.Vector" (Data.Vector.fromList . map fst)
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
           nf sum elems)
      | i <- [10, 100, 1000, 10000, 100000, 1000000]
      , Sum title fromList <- funcs
      ]