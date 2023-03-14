{-# LANGUAGE OverloadedStrings #-}
module Test.Data.Series.IO (tests) where

import           Data.Series          ( Series, at, readCSVFromFile )

import           Test.Tasty           ( testGroup, TestTree ) 
import           Test.Tasty.HUnit     ( testCase, assertEqual )

tests :: TestTree
tests = testGroup "Data.Series.IO" [ testReadCSVFromFile ]


testReadCSVFromFile :: TestTree
testReadCSVFromFile = testCase "Read CSV data" $ do
    (latitudes  :: Series String Double) <- either (error . show) id <$> readCSVFromFile "test/data/lat-long-city.csv" "city" "latitude"
    (longitudes :: Series String Double) <- either (error . show) id <$> readCSVFromFile "test/data/lat-long-city.csv" "city" "longitude"

    assertEqual mempty 4 (length latitudes)
    assertEqual mempty (Just 48.856667) (latitudes `at` "Paris") 
    assertEqual mempty (Just 40.712778) (latitudes `at` "New York City") 
    assertEqual mempty (Just 25.0375) (latitudes `at` "Taipei") 
    assertEqual mempty (Just (-34.603333)) (latitudes `at` "Buenos Aires") 

    assertEqual mempty 4 (length longitudes)
    assertEqual mempty (Just 2.352222) (longitudes `at` "Paris") 
    assertEqual mempty (Just (-74.006111)) (longitudes `at` "New York City") 
    assertEqual mempty (Just 121.5625) (longitudes `at` "Taipei") 
    assertEqual mempty (Just (-58.381667)) (longitudes `at` "Buenos Aires") 
