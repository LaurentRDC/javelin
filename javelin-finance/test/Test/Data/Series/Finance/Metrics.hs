module Test.Data.Series.Finance.Metrics ( tests ) where

import           Control.Monad        ( guard )
import           Data.Series.Finance.Metrics  ( compute, sharpeRatio, maxDrawDown, sortinoRatio )
import qualified Data.Series          as Series
import qualified Data.Vector          as Vector

import           Hedgehog             ( property, forAll, assert )
import qualified Hedgehog.Gen         as Gen
import qualified Hedgehog.Range       as Range

import qualified Statistics.Sample    as Stats
import           Test.Tasty           ( testGroup, TestTree ) 
import           Test.Tasty.Hedgehog  ( testProperty )
import           Test.Tasty.HUnit     ( testCase, assertEqual )
import           Test.Utils           ( approx, assertApproxEqual )


tests :: TestTree
tests = testGroup "Data.Series.Finance.Metrics" 
    [ testSharpeRatio
    , testSortinoRatio
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
            assertApproxEqual mempty 1e-8 (2 * sqrt 2) (xs `compute` sharpeRatio)
        
        testProp = testProperty "sharpeRatio is mean/std" $ property $ do
            ms <- forAll $ Gen.list (Range.linear 0 100) (Gen.double $ Range.linearFrac (-500) 500)
            let xs = Series.fromList $ zip [(0::Int)..] ms
                vs = Vector.fromList ms
            
            -- Note that the std implementation of javelin
            -- uses a single-pass std algorithm, which is why
            -- the property below uses Stats.fastStdDev.
            xs `compute` sharpeRatio `approx` (Stats.mean vs / Stats.fastStdDev vs)


testSortinoRatio :: TestTree
testSortinoRatio = testGroup "sortinoRatio" 
                [ testBasicCase
                , testReturnsNanIfEmpty
                , testNoNegReturns
                , testProp
                ]
    where
        testBasicCase = testCase "Sortino ratio of [1,-2,3,-1]" $ do
            let xs = Series.fromList $ zip [(0::Int)..] [1.0 :: Double, -2.0, 3.0, -1.0]
            assertApproxEqual mempty 1e-8 (1/2) (xs `compute` sortinoRatio)
        
        testReturnsNanIfEmpty = testCase "Sortino ratio of empty series is NaN" $ do
            assertEqual mempty True (isNaN $ (mempty :: Series.Series Int Double) `compute` sortinoRatio)

        testNoNegReturns = testCase "Sortino ratio of non-empty series with no negative returns is NaN" $ do
            let xs = Series.fromList $ zip [(0::Int)..] [1.0 :: Double, 2.0, 3.0, 4.0]
            assertEqual mempty True (isNaN $ xs `compute` sortinoRatio)
        
        testProp = testProperty "Sortino is mean(x)/std(x[x<0])" $ property $ do
            ms <- forAll $ Gen.list (Range.linear 0 100) (Gen.double $ Range.linearFrac (-500) 500)
            -- If there are no negative returns, the result is NaN which isn't comparable.
            guard $ any (<0) ms
            let xs = Series.fromList $ zip [(0::Int)..] ms
                vs = Vector.fromList ms
            
            -- Note that the std implementation of javelin
            -- uses a single-pass std algorithm, which is why
            -- the property below uses Stats.fastStdDev.
            xs `compute` sortinoRatio `approx` (Stats.mean vs / Stats.fastStdDev (Vector.filter (<0) vs))


testMaxDrawDown :: TestTree
testMaxDrawDown = testGroup "maxDrawDown" 
                [ testNonPositiveMDDs
                , runCase [1, -1]             (-1)
                , runCase [0, 0, 0, 1, -1, 1] (-1)
                , runCase [1, -1, -1, 1, 1]   (-2)
                , runCase [1, 1, -1, -2, 0, -1, 2, 2, 1, -1, 0, 0, 0, 0, 0, -4, 0, 0, 2, 2, 1, -2, 10] (-5)
                ]
    where
        runCase :: [Double] -> Double -> TestTree
        runCase pnl expectation = testCase mempty $ do
            let xs = Series.fromList $ zip [0::Int ..] pnl
            assertEqual mempty expectation (xs `compute` maxDrawDown)
                
        testNonPositiveMDDs = testProperty "maxDrawDown is always non-positive" $ property $ do
            ms <- forAll $ Gen.list (Range.linear 0 100) (Gen.double $ Range.linearFrac (-500) 500)
            let xs = Series.fromList $ zip [(0::Int)..] ms
            assert (xs `compute` maxDrawDown <= 0)
