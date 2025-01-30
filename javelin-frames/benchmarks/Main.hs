{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}

import           Control.DeepSeq    ( NFData, rnf )
import           Control.Exception  ( evaluate )
import           Criterion.Main     ( bench, bgroup, nf, defaultMain )

import           Data.Function (on)
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
        reversed = Frame.fromRows $ Vector.reverse rs
    evaluate $ rnf rs
    evaluate $ rnf fr
    evaluate $ rnf reversed
    defaultMain
        [ bgroup "Row-wise operations" 
          [ bench "fromRows" $ nf (Frame.fromRows) rs
          , bench "toRows"   $ nf (Frame.toRows) fr
          , bench "toRows . fromRows" $ nf (Frame.fromRows . Frame.toRows) fr
          , bench "fromRows . toRows" $ nf (Frame.toRows . Frame.fromRows) rs
          , bench "sortRowsBy" $ nf (Frame.sortRowsBy (compare `on` field1)) reversed
          , bench "sortRowsByKey" $ nf (Frame.sortRowsByKey) reversed
          ]
        , bgroup "Lookups" 
          [ bench "lookup"   $ nf (Frame.lookup 100) fr 
          , bench "ilookup"  $ nf (Frame.ilookup 99) fr 
          , bench "at"       $ nf (`Frame.at` (100, field5)) fr 
          , bench "iat"      $ nf (`Frame.iat` (99, field5)) fr 
          ]
        , bgroup "Merging" 
            [ bench "mergeWithStrategy" $ nf (Frame.mergeWithStrategy (Frame.matchedStrategy (\_ r1 _ -> r1)) fr) reversed
            ]
        ]