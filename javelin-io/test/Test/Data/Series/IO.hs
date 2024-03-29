{-# LANGUAGE OverloadedStrings #-}
module Test.Data.Series.IO (tests) where

import           Data.Csv             ( FromNamedRecord(..), (.:), ToNamedRecord(..), (.=), namedRecord )
import           Data.Series.Generic  ( Series, at)
import qualified Data.Series.Generic  as Series
import           Data.Series.IO       ( readCSVFromFile, writeCSVToFile )
import           Data.String          ( IsString )
import           Data.Vector          ( Vector )

import           System.FilePath      ( (</>) )
import           System.IO.Temp       ( withSystemTempDirectory )

import           Test.Tasty           ( testGroup, TestTree ) 
import           Test.Tasty.HUnit     ( testCase, assertEqual )

tests :: TestTree
tests = testGroup "Data.Series.IO" [ testReadCSVFromFile
                                   , testWriteCSVToFile
                                   ]


data LatLong = MkLatLong {lat :: Double, long :: Double} deriving (Eq, Show)

instance FromNamedRecord LatLong where
    parseNamedRecord r = MkLatLong <$> r .: "latitude"
                                   <*> r .: "longitude"

instance ToNamedRecord LatLong where
    toNamedRecord (MkLatLong lat long) = namedRecord [ "latitude" .= lat
                                                     , "longitude" .= long
                                                     ]

newtype City = MkCity String
    deriving (Eq, Ord, IsString, Show) 

instance FromNamedRecord City where
    parseNamedRecord r = MkCity <$> r .: "city"

instance ToNamedRecord City where
    toNamedRecord (MkCity city) = namedRecord ["city" .= city]


testReadCSVFromFile :: TestTree
testReadCSVFromFile = testCase "Read CSV data" $ do
    (latlongs  :: Series Vector City LatLong) <- either error id <$> readCSVFromFile "test/data/lat-long-city.csv"

    let latitudes  = fmap lat latlongs
        longitudes = fmap long latlongs

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


testWriteCSVToFile :: TestTree
testWriteCSVToFile = testCase "Write CSV data" $ do
    (latlongs  :: Series Vector City LatLong) <- either error id <$> readCSVFromFile "test/data/lat-long-city.csv"
    
    (written :: Series Vector City LatLong) <- withSystemTempDirectory "test-javelin-op" $ \dirname -> do
        writeCSVToFile (dirname </> "myseries.csv") latlongs

        either error id <$> readCSVFromFile (dirname </> "myseries.csv")
    
    assertEqual mempty latlongs written
