{-# LANGUAGE DerivingVia #-}
module Data.Series.IO (
    ColumnName,
    readCSV,
    readCSVFromFile,

    columns,
    columnsFromFile,
) where


import           Control.Monad          ( forM )
import           Data.Bifunctor         ( Bifunctor(first) )
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BL
import           Data.Coerce            ( coerce )
import           Data.Csv               ( FromField, (.:) )
import qualified Data.Csv               as CSV
import           Data.Functor           ( (<&>) )
import           Data.String            ( IsString )
import           Data.Series.Definition ( Series, fromList )
import           Data.Text              ( Text, pack )
import           Data.Vector            ( Vector )
import qualified Data.Vector            as Vector
import qualified System.IO              as IO


newtype ColumnName = MkColumnName BS.ByteString
    deriving (Eq, IsString, Show) via BS.ByteString


readCSV :: (Ord k, FromField k, FromField a)
        => ColumnName -- ^ Index column
        -> ColumnName -- ^ Values volumn
        -> BL.ByteString
        -> Either Text (Series k a)
readCSV indexCol dataCol bytes = first pack $ do
    (_, records :: Vector CSV.NamedRecord) <- CSV.decodeByName bytes
    let indexColName = coerce indexCol
        dataColName  = coerce dataCol

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


readCSVFromFile :: (Ord k, FromField k, FromField a)
                => FilePath
                -> ColumnName -- ^ Index column
                -> ColumnName -- ^ Values volumn
                -> IO (Either Text (Series k a))
readCSVFromFile fp indexCol valuesCol = fromFile fp (readCSV indexCol valuesCol) 


columns :: BL.ByteString -> Either Text [ColumnName]
columns bytes = first pack $ do
    (header, _ :: Vector CSV.NamedRecord) <- CSV.decodeByName bytes
    pure $ MkColumnName <$> Vector.toList header


columnsFromFile :: FilePath -> IO (Either Text [ColumnName])
columnsFromFile fp = fromFile fp columns