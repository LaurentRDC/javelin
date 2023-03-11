module Test.Data.Series.Numeric (tests) where

import           Data.AEq             ( AEq((~==)))
import           Data.Series          ( fromList, mean, variance, sampleVariance, std )
import qualified Data.Vector          as Vector          

import           Hedgehog             ( MonadTest, property, forAll, (===), assert, diff )
import qualified Hedgehog.Gen         as Gen
import qualified Hedgehog.Range       as Range
import           Hedgehog.Internal.Source ( HasCallStack, withFrozenCallStack ) 

import qualified Statistics.Sample    as Stats

import           Test.Tasty           ( testGroup, TestTree ) 
import           Test.Tasty.Hedgehog  ( testProperty )


tests :: TestTree
tests = testGroup "Data.Series.Numeric" [ testPropMean
                                        , testPropVariance
                                        , testSampleVariance
                                        , testPropStdDev
                                        ]

-- | Fails the test if the two arguments provided are not equal to within `epsilon`.
approx :: (MonadTest m, AEq a, Show a, HasCallStack) => a -> a -> m ()
approx x y =
  withFrozenCallStack $
    diff x (~==) y


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


testPropVariance :: TestTree
testPropVariance
    = testProperty "population variance" $ property $ do
        ms <- forAll $ Gen.list (Range.linear 0 100) (Gen.double $ Range.linearFrac (-500) 500) 
        let xs = fromList (zip [0::Int ..] ms)
        length xs === length ms 
        let v :: Double = variance xs
        -- IEEE 754 specifies that NaN != NaN...
        case length xs of
            0 -> assert $ isNaN v
            _ -> v `approx` Stats.fastVariance (Vector.fromList ms)


testSampleVariance :: TestTree
testSampleVariance
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