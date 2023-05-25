module Test.Data.Series.Finance.Metrics ( tests ) where

import           Data.Series.Finance.Metrics  ( sharpeRatio, maxDrawDown )
import qualified Data.Series.Unboxed  as Series
import qualified Data.Vector.Unboxed  as Vector

import           Hedgehog             ( property, forAll, assert )
import qualified Hedgehog.Gen         as Gen
import qualified Hedgehog.Range       as Range

import qualified Statistics.Sample    as Stats
import           Test.Tasty           ( testGroup, TestTree ) 
import           Test.Tasty.Hedgehog  ( testProperty )
import           Test.Tasty.HUnit     ( testCase, assertEqual )
import           Test.Utils           ( approx, assertApproxEqual )


tests :: TestTree
tests = testGroup "Data.Series.Finance.Metrics" [ testSharpeRatio
                                                , testMaxDrawDown
                                                ]


testSharpeRatio :: TestTree
testSharpeRatio = testGroup "sharpeRatio" 
                [ testBasicCase
                , testProp
                ]
    where
        testBasicCase = testCase "Sharpe ratio of [1,2,3,2]" $ do
            let xs = Series.fromList $ zip [(0::Int)..] [1.0 :: Double, 2.0, 3.0, 2.0]
            assertApproxEqual mempty 1e-8 (2 * sqrt 2) (sharpeRatio xs)
        
        testProp = testProperty "sharpeRatio is mean/std" $ property $ do
            ms <- forAll $ Gen.list (Range.linear 0 100) (Gen.double $ Range.linearFrac (-500) 500)
            let xs = Series.fromList $ zip [(0::Int)..] ms
                vs = Vector.fromList ms
            
            -- Note that the std implementation of javelin
            -- uses a single-pass std algorithm, which is why
            -- the property below uses Stats.fastStdDev.
            sharpeRatio xs `approx` (Stats.mean vs / Stats.fastStdDev vs)


testMaxDrawDown :: TestTree
testMaxDrawDown = testGroup "maxDrawDown" 
                [ testBasicCase
                , testMultipleDrawdowns
                , testNonPositiveMDDs
                ]
    where
        testBasicCase = testCase "maxDrawDown of simple case" $ do
            let xs = Series.fromList $ zip [(0::Int)..] [1 :: Int, -1, -1, 0, -1, 1]
            assertEqual mempty (-3) (maxDrawDown xs)

        testMultipleDrawdowns = testCase "maxDrawDown of two drawdowns" $ do
            let xs = Series.fromList $ zip [(0::Int)..] [1 :: Int, -1, -1, 0, -1, 1, -10]
            assertEqual mempty (-10) (maxDrawDown xs)
                
        testNonPositiveMDDs = testProperty "maxDrawDown is always non-positive" $ property $ do
            ms <- forAll $ Gen.list (Range.linear 0 100) (Gen.double $ Range.linearFrac (-500) 500)
            let xs = Series.fromList $ zip [(0::Int)..] ms
             in assert (maxDrawDown xs <= 0)
