module Test.Data.Series.Numeric (tests) where

import           Data.Series          ( fromList, mean, nanmean, var, nanvar, sampleVariance
                                      , nanSampleVariance, std, nanstd, meanAndVariance, nanMeanAndVariance 
                                      )
import qualified Data.Vector          as Vector          

import           Hedgehog             ( property, forAll, (===), assert )
import qualified Hedgehog.Gen         as Gen
import qualified Hedgehog.Range       as Range

import qualified Statistics.Sample    as Stats

import           Test.Tasty           ( testGroup, TestTree ) 
import           Test.Tasty.Hedgehog  ( testProperty )
import           Test.Utils           ( approx )


tests :: TestTree
tests = testGroup "Data.Series.Numeric" [ testPropMean
                                        , testPropNaNMean
                                        , testPropVariance
                                        , testPropNaNVariance
                                        , testPropSampleVariance
                                        , testPropNaNSampleVariance
                                        , testPropStdDev
                                        , testNaNPropStdDev
                                        , testPropMeanAndVariance
                                        , testPropNaNMeanAndVariance
                                        ]


testPropMean :: TestTree
testPropMean 
    = testProperty "mean" $ property $ do
        ms <- forAll $ Gen.list (Range.linear 0 100) (Gen.double $ Range.linearFrac (-500) 500) 
        let xs = fromList (zip [0::Int ..] ms)
        length xs === length ms 
        let m :: Double = mean xs
        -- IEEE 754 specifies that NaN != NaN...
        case length xs of
            0 -> assert $ isNaN m
            _ -> m `approx` Stats.mean (Vector.fromList ms)


testPropNaNMean :: TestTree
testPropNaNMean 
    = testProperty "nanmean" $ property $ do
        ms <- forAll $ Gen.list (Range.linear 0 100) (Gen.double $ Range.linearFrac (-500) 500) 
        let xs = fromList (zip [0::Int ..] ms)
        length xs === length ms 
        let m :: Double = nanmean xs
        -- IEEE 754 specifies that NaN != NaN...
        case length xs of
            0 -> m === 0
            _ -> m `approx` Stats.mean (Vector.fromList ms)


testPropVariance :: TestTree
testPropVariance
    = testProperty "population variance" $ property $ do
        ms <- forAll $ Gen.list (Range.linear 0 100) (Gen.double $ Range.linearFrac (-500) 500) 
        let xs = fromList (zip [0::Int ..] ms)
        length xs === length ms 
        let v :: Double = var xs
        -- IEEE 754 specifies that NaN != NaN...
        case length xs of
            0 -> assert $ isNaN v
            _ -> v `approx` Stats.fastVariance (Vector.fromList ms)


testPropNaNVariance :: TestTree
testPropNaNVariance
    = testProperty "population variance (ignoring NaN)" $ property $ do
        ms <- forAll $ Gen.list (Range.linear 0 100) (Gen.double $ Range.linearFrac (-500) 500) 
        let xs = fromList (zip [0::Int ..] ms)
        length xs === length ms 
        let v :: Double = nanvar xs
        -- IEEE 754 specifies that NaN != NaN...
        case length xs of
            0 -> v === 0
            _ -> v `approx` Stats.fastVariance (Vector.fromList ms)


testPropSampleVariance :: TestTree
testPropSampleVariance
    = testProperty "sample variance" $ property $ do
        ms <- forAll $ Gen.list (Range.linear 0 100) (Gen.double $ Range.linearFrac (-500) 500) 
        let xs = fromList (zip [0::Int ..] ms)
        length xs === length ms 
        let v :: Double = sampleVariance xs
        -- IEEE 754 specifies that NaN != NaN...
        case length xs of
            0 -> assert $ isNaN v
            1 -> assert $ isNaN v
            _ -> v `approx` Stats.fastVarianceUnbiased (Vector.fromList ms)


testPropNaNSampleVariance :: TestTree
testPropNaNSampleVariance
    = testProperty "sample variance (ignoring NaN)" $ property $ do
        ms <- forAll $ Gen.list (Range.linear 0 100) (Gen.double $ Range.linearFrac (-500) 500) 
        let xs = fromList (zip [0::Int ..] ms)
        length xs === length ms 
        let v :: Double = nanSampleVariance xs
        -- IEEE 754 specifies that NaN != NaN...
        case length xs of
            0 -> v === 0
            1 -> v === 0
            _ -> v `approx` Stats.fastVarianceUnbiased (Vector.fromList ms)


testPropStdDev :: TestTree
testPropStdDev
    = testProperty "population standard deviation" $ property $ do
        ms <- forAll $ Gen.list (Range.linear 0 100) (Gen.double $ Range.linearFrac (-500) 500) 
        let xs = fromList (zip [0::Int ..] ms)
        length xs === length ms 
        let d :: Double = std xs
        -- IEEE 754 specifies that NaN != NaN...
        case length xs of
            0 -> assert $ isNaN d
            _ -> d `approx` Stats.fastStdDev (Vector.fromList ms)


testNaNPropStdDev :: TestTree
testNaNPropStdDev
    = testProperty "population standard deviation (ignoring NaN)" $ property $ do
        ms <- forAll $ Gen.list (Range.linear 0 100) (Gen.double $ Range.linearFrac (-500) 500) 
        let xs = fromList (zip [0::Int ..] ms)
        length xs === length ms 
        let d :: Double = nanstd xs
        -- IEEE 754 specifies that NaN != NaN...
        case length xs of
            0 -> d === 0
            _ -> d `approx` Stats.fastStdDev (Vector.fromList ms)


testPropMeanAndVariance :: TestTree
testPropMeanAndVariance
    = testProperty "population standard deviation" $ property $ do
        ms <- forAll $ Gen.list (Range.linear 0 100) (Gen.double $ Range.linearFrac (-500) 500) 
        let xs = fromList (zip [0::Int ..] ms)
        length xs === length ms 
        let (m :: Double, v) = meanAndVariance xs
        -- IEEE 754 specifies that NaN != NaN...
        case length xs of
            0 -> assert $ isNaN m && isNaN v
            _ -> do
                m `approx` Stats.mean (Vector.fromList ms)
                v `approx` Stats.fastVariance (Vector.fromList ms)


testPropNaNMeanAndVariance :: TestTree
testPropNaNMeanAndVariance
    = testProperty "population standard deviation (ignoring NaN)" $ property $ do
        ms <- forAll $ Gen.list (Range.linear 0 100) (Gen.double $ Range.linearFrac (-500) 500) 
        let xs = fromList (zip [0::Int ..] ms)
        length xs === length ms 
        let (m :: Double, v) = nanMeanAndVariance xs
        -- IEEE 754 specifies that NaN != NaN...
        case length xs of
            0 -> do
                m === 0
                v === 0
            _ -> do
                m `approx` Stats.mean (Vector.fromList ms)
                v `approx` Stats.fastVariance (Vector.fromList ms)