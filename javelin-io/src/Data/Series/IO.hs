{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  $module
-- Copyright   :  (c) Laurent P. René de Cotret
-- License     :  MIT
-- Maintainer  :  laurent.decotret@outlook.com
-- Portability :  portable
--
-- This module contains functions to serialize/deserialize 'Series'
-- to/from bytes.
module Data.Series.IO (
    -- * Deserialize 'Series'
    readCSV,
    readCSVFromFile,

    -- * Serialize 'Series'
    writeCSV,
    writeCSVToFile,
) where


import           Control.Monad.IO.Class ( MonadIO )
import qualified Data.ByteString.Lazy   as BL
import           Data.Csv               ( FromNamedRecord(..), ToNamedRecord(..), )
import           Data.Series            ( Series )
import qualified Data.Series.Generic.IO as Generic.IO


{-|
Read a comma-separated value (CSV) bytestream into a series.

Consider the following bytestream read from a file:

@
latitude,longitude,city
48.856667,2.352222,Paris
40.712778,-74.006111,New York City
25.0375,121.5625,Taipei
-34.603333,-58.381667,Buenos Aires
@

We want to get a series of the latitude an longitude, indexed by the column "city". First, we need
to do is to create a datatype representing the latitude and longitude information, and our index:

@
data LatLong = MkLatLong { latitude  :: Double
                         , longitude :: Double
                         }
    deriving ( Show )

newtype City = MkCity String
    deriving ( Eq, Ord, Show )
@

Second, we need to create an instance of `Data.Csv.FromNamedRecord` for our new types:

@
import "Data.Csv" ( 'FromNamedRecord', '(.:)' )

instance 'FromNamedRecord' LatLong where
    'parseNamedRecord' r = MkLatLong \<$\> r .: "latitude"
                                   \<*\> r .: "longitude"


instance 'FromNamedRecord' City where
    'parseNamedRecord' r = MkCity \<$\> r .: "city"
@

Finally, we're ready to read our stream:

@
import "Data.Series"
import "Data.Series.IO"

main :: IO ()
main = do
    let fp = "path/to/my/file.csv"
    let (latlongs  :: 'Series' City LatLong) = either (error . show) id \<$\> `readCSVFromFile` fp
    print latlongs
@
-}
readCSV :: (Ord k, FromNamedRecord k, FromNamedRecord a)
        => BL.ByteString
        -> Either String (Series k a)
readCSV = Generic.IO.readCSV


{-|
This is a helper function to read a CSV directly from a filepath.
See the documentation for 'readCSV' on how to prepare your types.
Then, for example, you can use 'readCSVFromFile' as:

@
import "Data.Series"
import "Data.Series.IO"

main :: IO ()
main = do
    let fp = "path/to/my/file.csv"
    let (latlongs  :: 'Series' City LatLong) = either (error . show) id \<$\> `readCSVFromFile` fp
    print latlongs
@
-}
readCSVFromFile :: (MonadIO m, Ord k, FromNamedRecord k, FromNamedRecord a)
                => FilePath
                -> m (Either String (Series k a))
readCSVFromFile = Generic.IO.readCSVFromFile



{-|
Read a comma-separated value (CSV) bytestream into a series.

Consider the following bytestream read from a file:

@
latitude,longitude,city
48.856667,2.352222,Paris
40.712778,-74.006111,New York City
25.0375,121.5625,Taipei
-34.603333,-58.381667,Buenos Aires
@

We want to get a series of the latitude an longitude, indexed by the column "city". First, we need
to do is to create a datatype representing the latitude and longitude information, and our index:

@
data LatLong = MkLatLong { latitude  :: Double
                         , longitude :: Double
                         }
    deriving ( Show )

newtype City = MkCity String
    deriving ( Eq, Ord, Show )
@

Second, we need to create an instance of `Data.Csv.FromNamedRecord` for our new types:

@
import "Data.Csv" ( 'FromNamedRecord', '(.:)' )

instance 'FromNamedRecord' LatLong where
    'parseNamedRecord' r = MkLatLong \<$\> r .: "latitude"
                                   \<*\> r .: "longitude"


instance 'FromNamedRecord' City where
    'parseNamedRecord' r = MkCity \<$\> r .: "city"
@

Finally, we're ready to read our stream:

@
import Data.Series
import Data.Series.IO

main :: IO ()
main = do
    stream <- (...) -- Read the bytestring from somewhere
    let (latlongs  :: 'Series' City LatLong) = either (error . show) id \<$\> `readCSV` stream
    print latlongs
@
-}
writeCSV :: (ToNamedRecord k, ToNamedRecord a)
         => Series k a
         -> BL.ByteString
writeCSV = Generic.IO.writeCSV


-- | This is a helper function to write a 'Series' directly to CSV.
-- See the documentation for 'writeCSV' on how to prepare your types.
writeCSVToFile :: (MonadIO m, ToNamedRecord k, ToNamedRecord a)
               => FilePath
               -> Series k a
               -> m ()
writeCSVToFile = Generic.IO.writeCSVToFile
