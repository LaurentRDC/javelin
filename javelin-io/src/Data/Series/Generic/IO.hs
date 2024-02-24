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
-- This module contains functions to serialize/deserialize generic 'Series'
-- to/from bytes.
--
-- Use this module if you want to support all types of 'Series'. Otherwise,
-- you should use either the modules "Data.Series.IO" or "Data.Series.Unboxed.IO". 
module Data.Series.Generic.IO (
    -- * Deserialize 'Series'
    readCSV,
    readCSVFromFile,

    -- * Serialize 'Series'
    writeCSV,
    writeCSVToFile,
) where


import           Control.Monad          ( forM )
import           Control.Monad.IO.Class ( MonadIO(liftIO) )
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BL
import           Data.Csv               ( FromNamedRecord(..), ToNamedRecord(..), )
import qualified Data.Csv               as CSV
import           Data.Functor           ( (<&>) )
import qualified Data.HashMap.Strict    as HashMap
import qualified Data.List.NonEmpty     as NE
import           Data.Maybe             ( fromMaybe )
import           Data.Series.Generic    ( Series, fromVector, convert )
import qualified Data.Series.Generic    as GSeries
import qualified Data.Vector            as Boxed
import           Data.Vector.Generic    ( Vector )
import qualified System.IO              as IO


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
import "Data.Series.Generic"
import "Data.Series.Generic.IO"
import "Data.Vector" 

main :: IO ()
main = do
    stream <- (...) -- Read the bytestring from somewhere
    let (latlongs  :: 'Series' Vector City LatLong) = either (error . show) id \<$\> `readCSV` stream
    print latlongs
@
-}
readCSV :: (Vector v a, Ord k, FromNamedRecord k, FromNamedRecord a)
        => BL.ByteString
        -> Either String (Series v k a)
readCSV bytes = do
    (_, records :: Boxed.Vector CSV.NamedRecord) <- CSV.decodeByName bytes

    rows <- CSV.runParser $ forM records
                          $ \record -> (,) <$> parseNamedRecord record
                                           <*> parseNamedRecord record

    pure $ convert $ fromVector rows


fromFile :: MonadIO m 
         => FilePath
         -> (BL.ByteString -> Either String b)
         -> m (Either String b)
fromFile fp f
    = liftIO $ IO.withFile fp IO.ReadMode $ \h -> do
        IO.hSetBinaryMode h True
        BS.hGetContents h <&> f . BL.fromStrict


{-|
This is a helper function to read a CSV directly from a filepath.
See the documentation for 'readCSV' on how to prepare your types.
Then, for example, you can use 'readCSVFromFile' as:

@
import "Data.Series.Generic"
import "Data.Series.Generic.IO"
import "Data.Vector" 

main :: IO ()
main = do
    stream <- (...) -- Read the bytestring from somewhere
    let (latlongs  :: 'Series' Vector City LatLong) = either (error . show) id \<$\> `readCSV` stream
    print latlongs
@
-}
readCSVFromFile :: (MonadIO m, Vector v a, Ord k, FromNamedRecord k, FromNamedRecord a)
                => FilePath
                -> m (Either String (Series v k a))
readCSVFromFile fp = fromFile fp readCSV 



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
writeCSV :: (Vector v a, ToNamedRecord k, ToNamedRecord a)
         => Series v k a
         -> BL.ByteString
writeCSV xs = fromMaybe mempty $ do
    recs   <- NE.nonEmpty [ toNamedRecord k <> toNamedRecord v | (k, v) <- GSeries.toList xs]
    let header =  CSV.header $ HashMap.keys $ NE.head recs
    pure $ CSV.encodeByName header $ NE.toList recs


-- | This is a helper function to write a 'Series' directly to CSV.
-- See the documentation for 'writeCSV' on how to prepare your types.
writeCSVToFile :: (MonadIO m, Vector v a, ToNamedRecord k, ToNamedRecord a)
               => FilePath
               -> Series v k a
               -> m ()
writeCSVToFile fp 
    = liftIO 
    . BS.writeFile fp 
    . BL.toStrict 
    . writeCSV