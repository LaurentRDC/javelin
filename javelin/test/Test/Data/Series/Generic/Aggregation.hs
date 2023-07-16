
module Test.Data.Series.Generic.Aggregation (tests) where

import qualified Data.Map.Strict      as MS
import qualified Data.Series.Generic  as Series
import           Data.Series.Generic  ( Series, fromStrictMap, groupBy, foldGroups, rollingForwards, rollingBackwards, expanding)
import           Data.Vector          ( Vector )

import           Hedgehog             ( property, forAll, (===) )
import qualified Hedgehog.Gen         as Gen
import qualified Hedgehog.Range       as Range

import           Prelude              hiding ( zipWith )

import           Test.Tasty           ( testGroup, TestTree )
import           Test.Tasty.Hedgehog  ( testProperty )
import           Test.Tasty.HUnit     ( testCase, assertEqual )

tests :: TestTree
tests = testGroup "Data.Series.Generic.Aggregation" [ testGroupBy
                                                    , testRollingForwards
                                                    , testRollingBackwards
                                                    , testPropaggregateVsfoldGroups
                                                    , testExpanding
                                                    ]


testGroupBy :: TestTree
testGroupBy = testGroup "Data.Series.Generic.groupBy" [ testGroupBy1, testGroupBy2 ]
    where
        testGroupBy1 = testCase "groupBy" $ do
            let (series :: Series Vector String Int) = fromStrictMap $ MS.fromList [("aa", 1), ("ab", 2), ("c", 3), ("dc", 4), ("ae", 5)]
                expectation = fromStrictMap $ MS.fromList [('a', 1+2+5), ('c', 3), ('d', 4)]
            
            assertEqual mempty expectation $ groupBy head (Series.sum :: Series Vector String Int -> Int) series

        testGroupBy2 = testCase "groupBy" $ do
            let (series :: Series Vector Int Int) = fromStrictMap $ MS.fromList $ zip [0,1,2,3] [0,1,2,3]
                expectation = fromStrictMap $ MS.fromList [(True, 0+2), (False, 1+3)]
            
            assertEqual mempty expectation $ groupBy even (Series.sum :: Series Vector Int Int -> Int) series


testRollingForwards :: TestTree
testRollingForwards = testGroup "Data.Series.Generic.rollingForwards" [ test1, test2 ]
    where
        test1 = testCase "rollingForwards" $ do
            let (series :: Series Vector Int Int) = fromStrictMap $ MS.fromList [(1, 1), (2, 2), (3, 3), (4, 4), (5, 5)]
                expectation = fromStrictMap $ MS.fromList [ (1, 1+2)
                                                          , (2, 2+3)
                                                          , (3, 3+4)
                                                          , (4, 4+5)
                                                          , (5, 5)
                                                          ]
            
            assertEqual mempty expectation $ rollingForwards 1 (Series.sum :: Series Vector Int Int -> Int) series

        test2 = testCase "rollingForwards" $ do
            let (series :: Series Vector Int Int) = fromStrictMap $ MS.fromList [(1, 1), (2, 2), (3, 3), (4, 4), (5, 5)]
                expectation = fromStrictMap $ MS.fromList [ (1, 1+2+3)
                                                          , (2, 2+3+4)
                                                          , (3, 3+4+5)
                                                          , (4, 4+5)
                                                          , (5, 5)
                                                          ]
            
            assertEqual mempty expectation $ rollingForwards 2 (Series.sum :: Series Vector Int Int -> Int) series


testRollingBackwards :: TestTree
testRollingBackwards = testGroup "Data.Series.Generic.rollingBackwards" [ test1, test2 ]
    where
        test1 = testCase "rollingForwards" $ do
            let (series :: Series Vector Int Int) = fromStrictMap $ MS.fromList [(1, 1), (2, 2), (3, 3), (4, 4), (5, 5)]
                expectation = fromStrictMap $ MS.fromList [ (1, 1)
                                                          , (2, 1+2)
                                                          , (3, 2+3)
                                                          , (4, 3+4)
                                                          , (5, 4+5)
                                                          ]
            
            assertEqual mempty expectation $ rollingBackwards 1 (Series.sum :: Series Vector Int Int -> Int) series

        test2 = testCase "rollingForwards" $ do
            let (series :: Series Vector Int Int) = fromStrictMap $ MS.fromList [(1, 1), (2, 2), (3, 3), (4, 4), (5, 5)]
                expectation = fromStrictMap $ MS.fromList [ (1, 1)
                                                          , (2, 1+2)
                                                          , (3, 1+2+3)
                                                          , (4, 2+3+4)
                                                          , (5, 3+4+5)
                                                          ]
            
            assertEqual mempty expectation $ rollingBackwards 2 (Series.sum :: Series Vector Int Int -> Int) series


testPropaggregateVsfoldGroups :: TestTree
testPropaggregateVsfoldGroups 
    = testProperty "check that groupBy and testRollingForwards are equivalent" $ property $ do
        ms <- forAll $ Gen.list (Range.linear 0 100) (Gen.double $ Range.linearFrac (-500) 500) 
        let (xs :: Series Vector Int Double) = Series.fromList (zip [0::Int ..] ms)

        groupBy (`mod` 5) (Series.sum :: Series Vector Int Double -> Double) xs === foldGroups (`mod` 5) (+) xs


testExpanding :: TestTree
testExpanding = testCase "expanding" $ do
    let (xs :: Series Vector Char Int) = Series.fromList $ zip ['a', 'b', 'c', 'd'] [1::Int,2,3,4]
        rs = xs `expanding` Series.sum
        expectation = Series.fromList $ zip ['a', 'b', 'c', 'd'] [1,1+2,1+2+3,1+2+3+4]
    
    assertEqual mempty expectation rs