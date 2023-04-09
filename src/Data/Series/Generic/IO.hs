{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Series.Generic.IO (
    ColumnName(..),
    readCSV,
    readCSVFromFile,

    columns,
    columnsFromFile,

    readJSON,
    readJSONFromFile,
) where


import           Control.Monad          ( forM )
import           Data.Aeson             ( FromJSONKey, FromJSON )
import qualified Data.Aeson             as JSON
import           Data.Bifunctor         ( Bifunctor(first, bimap) )
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BL
import           Data.Coerce            ( coerce )
import           Data.Csv               ( FromField, (.:) )
import qualified Data.Csv               as CSV
import           Data.Functor           ( (<&>) )
import           Data.Map.Strict        ( Map )
import           Data.String            ( IsString )
import           Data.Series.Generic.Definition ( Series, fromList, fromStrictMap )
import           Data.Text              ( Text, pack )
import qualified Data.Text.Encoding     as Text
import qualified Data.Vector            as Boxed
import           Data.Vector.Generic    ( Vector )
import qualified Data.Vector.Generic    as Vector
import qualified System.IO              as IO


newtype ColumnName = MkColumnName Text
    deriving (Eq, Ord, IsString, FromJSONKey, Show) via Text


readCSV :: (Vector v a, Ord k, FromField k, FromField a)
        => ColumnName -- ^ Index column
        -> ColumnName -- ^ Values volumn
        -> BL.ByteString
        -> Either Text (Series v k a)
readCSV indexCol dataCol bytes = first pack $ do
    (_, records :: Boxed.Vector CSV.NamedRecord) <- CSV.decodeByName bytes
    let indexColName = Text.encodeUtf8 $ coerce indexCol
        dataColName  = Text.encodeUtf8 $ coerce dataCol

    rows <- CSV.runParser $ forM (Vector.toList records)
                          $ \record -> (,) <$> record .: indexColName
                                           <*> record .: dataColName

    pure $ fromList rows


fromFile :: FilePath
         -> (BL.ByteString -> Either Text b)
         -> IO (Either Text b)
fromFile fp f
    = IO.withFile fp IO.ReadMode $ \h -> do
        IO.hSetBinaryMode h True
        (BS.hGetContents h <&> BL.fromStrict) <&> f


readCSVFromFile :: (Vector v a, Ord k, FromField k, FromField a)
                => FilePath
                -> ColumnName -- ^ Index column
                -> ColumnName -- ^ Values column
                -> IO (Either Text (Series v k a))
readCSVFromFile fp indexCol valuesCol = fromFile fp (readCSV indexCol valuesCol) 


columns :: BL.ByteString -> Either Text [ColumnName]
columns bytes = first pack $ do
    (header, _ :: Boxed.Vector CSV.NamedRecord) <- CSV.decodeByName bytes
    pure $ MkColumnName . Text.decodeUtf8Lenient <$> Vector.toList header


columnsFromFile :: FilePath -> IO (Either Text [ColumnName])
columnsFromFile fp = fromFile fp columns


readJSON :: (Vector v a, Ord k, FromJSONKey k, FromJSON a) 
         => BL.ByteString 
         -> Either Text (Map ColumnName (Series v k a))
readJSON bytes = bimap pack (fmap fromStrictMap) 
               $ JSON.eitherDecode' bytes


readJSONFromFile :: (Vector v a, Ord k, FromJSONKey k, FromJSON a) 
                 => FilePath 
                 -> IO (Either Text (Map ColumnName (Series v k a)))
readJSONFromFile fp = fromFile fp readJSON