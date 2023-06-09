{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Series.IO (
    ColumnName(..),
    readCSV,
    readCSVFromFile,

    writeCSV,
    writeCSVToFile,

    readJSON,
    readJSONFromFile,
) where


import           Control.Monad          ( forM )
import           Control.Monad.IO.Class ( MonadIO(liftIO) )
import           Data.Aeson             ( FromJSONKey, FromJSON )
import qualified Data.Aeson             as JSON
import           Data.Bifunctor         ( Bifunctor(second) )
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BL
import           Data.Csv               ( FromNamedRecord(..), ToNamedRecord(..), )
import qualified Data.Csv               as CSV
import           Data.Functor           ( (<&>) )
import qualified Data.HashMap.Strict    as HashMap
import           Data.Map.Strict        ( Map )
import           Data.String            ( IsString )
import           Data.Series.Generic    ( Series, fromVector, convert, fromStrictMap )
import qualified Data.Series.Generic    as GSeries
import           Data.Text              ( Text )
import qualified Data.Vector            as Boxed
import           Data.Vector.Generic    ( Vector )
import qualified System.IO              as IO


newtype ColumnName = MkColumnName Text
    deriving (Eq, Ord, IsString, FromJSONKey, Show) via Text


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
import `Data.Csv` ( `FromNamedRecord`, `(.:)` )

instance `FromNamedRecord` LatLong where
    `parseNamedRecord` r = MkLatLong \<$\> r .: "latitude"
                                   \<*\> r .: "longitude"


instance `FromNamedRecord` City where
    `parseNamedRecord` r = MkCity \<$\> r .: "city"
@

Finally, we're ready to read our stream:

@
import Data.Series
import Data.Series.IO

main :: IO ()
main = do
    stream <- (...) -- Read the bytestring from somewhere
    let (latlongs  :: `Series` City LatLong) = either (error . show) id \<$\> `readCSV` stream
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
        (BS.hGetContents h <&> BL.fromStrict) <&> f


{-|
This is a helper function to read a CSV directly from a filepath.
See the documentation for `readCSV` on hour to prepare your types.
Then, for example, you can use `readCSVFromFile` as:

@
import Data.Series
import Data.Series.IO

main :: IO ()
main = do
    let fp = "path/to/my/file.csv"
    let (latlongs  :: `Series` City LatLong) = either (error . show) id \<$\> `readCSVFromFile` fp
    print latlongs
@
-}
readCSVFromFile :: (MonadIO m, Vector v a, Ord k, FromNamedRecord k, FromNamedRecord a)
                => FilePath
                -> m (Either String (Series v k a))
readCSVFromFile fp = fromFile fp readCSV 



writeCSV :: (Vector v a, ToNamedRecord k, ToNamedRecord a)
         => Series v k a
         -> BL.ByteString
writeCSV xs 
    | GSeries.null xs = mempty
    | otherwise       = let recs   = [ toNamedRecord k <> toNamedRecord v | (k, v) <- GSeries.toList xs]
                            header =  CSV.header $ HashMap.keys $ head recs
                         in CSV.encodeByName header recs


writeCSVToFile :: (MonadIO m, Vector v a, ToNamedRecord k, ToNamedRecord a)
               => FilePath
               -> Series v k a
               -> m ()
writeCSVToFile fp xs = liftIO $ BS.writeFile fp $ BL.toStrict $ writeCSV xs



readJSON :: (Vector v a, Ord k, FromJSONKey k, FromJSON a) 
         => BL.ByteString 
         -> Either String (Map ColumnName (Series v k a))
readJSON bytes = second (fmap fromStrictMap) 
               $ JSON.eitherDecode' bytes


readJSONFromFile :: (Vector v a, Ord k, FromJSONKey k, FromJSON a) 
                 => FilePath 
                 -> IO (Either String (Map ColumnName (Series v k a)))
readJSONFromFile fp = fromFile fp readJSON