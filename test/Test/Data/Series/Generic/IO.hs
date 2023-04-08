{-# LANGUAGE OverloadedStrings #-}
module Test.Data.Series.Generic.IO (tests) where

import           Data.Map.Strict      ( Map )
import qualified Data.Map.Strict      as Map
import           Data.Series.Generic  ( Series, ColumnName, at, fromList, readCSVFromFile, columnsFromFile, readJSONFromFile )
import qualified Data.Series.Generic  as Series
import           Data.Vector          ( Vector )

import           Test.Tasty           ( testGroup, TestTree ) 
import           Test.Tasty.HUnit     ( testCase, assertEqual )

tests :: TestTree
tests = testGroup "Data.Series.Generic.IO" [ testReadCSVFromFile
                                           , testColumnsFromFile 
                                           , testReadJSONFromFile
                                           ]


testReadCSVFromFile :: TestTree
testReadCSVFromFile = testCase "Read CSV data" $ do
    (latitudes  :: Series Vector String Double) <- either (error . show) id <$> readCSVFromFile "test/data/lat-long-city.csv" "city" "latitude"
    (longitudes :: Series Vector String Double) <- either (error . show) id <$> readCSVFromFile "test/data/lat-long-city.csv" "city" "longitude"

    assertEqual mempty 4 (Series.length latitudes)
    assertEqual mempty (Just 48.856667) (latitudes `at` "Paris") 
    assertEqual mempty (Just 40.712778) (latitudes `at` "New York City") 
    assertEqual mempty (Just 25.0375) (latitudes `at` "Taipei") 
    assertEqual mempty (Just (-34.603333)) (latitudes `at` "Buenos Aires") 

    assertEqual mempty 4 (Series.length longitudes)
    assertEqual mempty (Just 2.352222) (longitudes `at` "Paris") 
    assertEqual mempty (Just (-74.006111)) (longitudes `at` "New York City") 
    assertEqual mempty (Just 121.5625) (longitudes `at` "Taipei") 
    assertEqual mempty (Just (-58.381667)) (longitudes `at` "Buenos Aires") 


testColumnsFromFile :: TestTree
testColumnsFromFile = testCase "Read columns in CSV file" $ do
    columns <- either (error . show) id <$> columnsFromFile "test/data/lat-long-city.csv"
    assertEqual mempty ["latitude", "longitude", "city"] columns


testReadJSONFromFile :: TestTree
testReadJSONFromFile = testCase "Read JSON data" $ do
    (mp :: Map ColumnName (Series Vector String Int)) <- either (error . show) id <$> readJSONFromFile "test/data/columns.json"

    let expectation = Map.fromList [ ("a", fromList [ ("hello", 1)
                                                    , ("world", 2)
                                                    ]
                                     )
                                   , ("b", fromList [ ("hello", 3)
                                                    , ("world", 4)
                                                    ]
                                     ) 
                                   ]

    assertEqual mempty expectation mp