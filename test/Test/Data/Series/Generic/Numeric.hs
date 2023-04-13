module Test.Data.Series.Generic.Numeric (tests) where

import           Data.Series.Generic  ( Series, fromList, mean, var, sampleVariance
                                      , std, meanAndVariance 
                                      )
import qualified Data.Series.Generic  as Series
import           Data.Vector          ( Vector )
import qualified Data.Vector          as Vector          

import           Hedgehog             ( property, forAll, (===), assert )
import qualified Hedgehog.Gen         as Gen
import qualified Hedgehog.Range       as Range

import qualified Statistics.Sample    as Stats

import           Test.Tasty           ( testGroup, TestTree ) 
import           Test.Tasty.Hedgehog  ( testProperty )
import           Test.Utils           ( approx )


tests :: TestTree
tests = testGroup "Data.Series.Generic.Numeric" [ testPropMean
                                                , testPropVariance
                                                , testPropSampleVariance
                                                , testPropStdDev
                                                , testPropMeanAndVariance
                                                ]


testPropMean :: TestTree
testPropMean 
    = testProperty "mean" $ property $ do
        ms <- forAll $ Gen.list (Range.linear 0 100) (Gen.double $ Range.linearFrac (-500) 500) 
        let (xs :: Series Vector Int Double) = fromList (zip [0::Int ..] ms)
        Series.length xs === length ms 
        let m :: Double = mean xs
        -- IEEE 754 specifies that NaN != NaN...
        case Series.length xs of
            0 -> assert $ isNaN m
            _ -> m `approx` Stats.mean (Vector.fromList ms)


testPropVariance :: TestTree
testPropVariance
    = testProperty "population variance" $ property $ do
        ms <- forAll $ Gen.list (Range.linear 0 100) (Gen.double $ Range.linearFrac (-500) 500) 
        let (xs :: Series Vector Int Double) = fromList (zip [0::Int ..] ms)
        Series.length xs === length ms 
        let v :: Double = var xs
        -- IEEE 754 specifies that NaN != NaN...
        case Series.length xs of
            0 -> assert $ isNaN v
            _ -> v `approx` Stats.fastVariance (Vector.fromList ms)


testPropSampleVariance :: TestTree
testPropSampleVariance
    = testProperty "sample variance" $ property $ do
        ms <- forAll $ Gen.list (Range.linear 0 100) (Gen.double $ Range.linearFrac (-500) 500) 
        let (xs :: Series Vector Int Double) = fromList (zip [0::Int ..] ms)
        Series.length xs === length ms 
        let v :: Double = sampleVariance xs
        -- IEEE 754 specifies that NaN != NaN...
        case Series.length xs of
            0 -> assert $ isNaN v
            1 -> assert $ isNaN v
            _ -> v `approx` Stats.fastVarianceUnbiased (Vector.fromList ms)


testPropStdDev :: TestTree
testPropStdDev
    = testProperty "population standard deviation" $ property $ do
        ms <- forAll $ Gen.list (Range.linear 0 100) (Gen.double $ Range.linearFrac (-500) 500) 
        let (xs :: Series Vector Int Double) = fromList (zip [0::Int ..] ms)
        Series.length xs === length ms 
        let d :: Double = std xs
        -- IEEE 754 specifies that NaN != NaN...
        case Series.length xs of
            0 -> assert $ isNaN d
            _ -> d `approx` Stats.fastStdDev (Vector.fromList ms)


testPropMeanAndVariance :: TestTree
testPropMeanAndVariance
    = testProperty "population standard deviation" $ property $ do
        ms <- forAll $ Gen.list (Range.linear 0 100) (Gen.double $ Range.linearFrac (-500) 500) 
        let (xs :: Series Vector Int Double) = fromList (zip [0::Int ..] ms)
        Series.length xs === length ms 
        let (m :: Double, v) = meanAndVariance xs
        -- IEEE 754 specifies that NaN != NaN...
        case Series.length xs of
            0 -> assert $ isNaN m && isNaN v
            _ -> do
                m `approx` Stats.mean (Vector.fromList ms)
                v `approx` Stats.fastVariance (Vector.fromList ms)
