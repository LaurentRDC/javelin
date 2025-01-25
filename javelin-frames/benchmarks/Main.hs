{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}

import           Control.DeepSeq    ( NFData, rnf )
import           Control.Exception  ( evaluate )
import           Criterion.Main     ( bench, nf, defaultMain )

import           Data.Frame ( Column, Frameable, Indexable, Row, Frame )
import qualified Data.Frame as Frame
import qualified Data.Vector as Vector

import           GHC.Generics ( Generic )


data Bench t
    = MkBench { field1 :: Column t Int
              , field2 :: Column t Int
              , field3 :: Column t Int
              , field4 :: Column t Int
              , field5 :: Column t Int
              , field6 :: Column t Int
              }
    deriving (Generic, Frameable)

instance NFData (Row Bench)
instance NFData (Frame Bench)

instance Indexable Bench where
    type Key Bench = Int
    
    index = field1


main :: IO ()
main = do
    let rs = Vector.fromList [MkBench ix 0 0 0 0 0 | ix <- [0::Int .. 100_000]]
        fr = Frame.fromRows rs
    evaluate $ rnf rs
    evaluate $ rnf fr
    defaultMain
        [ bench "fromRows" $ nf (Frame.fromRows) rs
        , bench "toRows"   $ nf (Frame.toRows) fr
        , bench "lookup"   $ nf (Frame.lookup 100) fr 
        , bench "at"       $ nf (Frame.at (100, field5)) fr 
        ]