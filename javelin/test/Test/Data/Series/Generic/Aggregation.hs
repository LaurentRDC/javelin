
module Test.Data.Series.Generic.Aggregation (tests) where

import qualified Data.Map.Strict      as MS
import qualified Data.Series.Generic  as Series
import           Data.Series.Generic  ( Series, fromStrictMap, groupBy, foldGroups, windowing, to, expanding)
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
                                                    , testWindowing
                                                    , testWindowingRollingForwards
                                                    , testWindowingRollingBackwards
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



testWindowing :: TestTree
testWindowing = testCase "Data.Series.Generic.windowing" $ do

    let (xs :: Series Vector Int Int) 
         = Series.fromList [ (1, 0)
                           , (2, 1)
                           , (3, 2)
                           , (4, 3)
                           , (5, 4)
                           , (6, 5)
                           ]
        expectation = Series.fromList [ (1, 3)
                                      , (2, 6)
                                      , (3, 9)
                                      , (4, 12)
                                      , (5, 9)
                                      , (6, 5)
                                      ]
    assertEqual mempty expectation $ windowing (\k -> k `to` (k+2)) sum xs


testWindowingRollingForwards :: TestTree
testWindowingRollingForwards = testGroup "Data.Series.Generic.windowing" [ test1, test2 ]
    where
        test1 = testCase "rollingForwards" $ do
            let (series :: Series Vector Int Int) = fromStrictMap $ MS.fromList [(1, 1), (2, 2), (3, 3), (4, 4), (5, 5)]
                expectation = fromStrictMap $ MS.fromList [ (1, 1+2)
                                                          , (2, 2+3)
                                                          , (3, 3+4)
                                                          , (4, 4+5)
                                                          , (5, 5)
                                                          ]
            
            assertEqual mempty expectation $ windowing (\k -> k `to` (k + 1)) (Series.sum :: Series Vector Int Int -> Int) series

        test2 = testCase "rollingForwards" $ do
            let (series :: Series Vector Int Int) = fromStrictMap $ MS.fromList [(1, 1), (2, 2), (3, 3), (4, 4), (5, 5)]
                expectation = fromStrictMap $ MS.fromList [ (1, 1+2+3)
                                                          , (2, 2+3+4)
                                                          , (3, 3+4+5)
                                                          , (4, 4+5)
                                                          , (5, 5)
                                                          ]
            
            assertEqual mempty expectation $ windowing (\k -> k `to` (k + 2)) (Series.sum :: Series Vector Int Int -> Int) series


testWindowingRollingBackwards :: TestTree
testWindowingRollingBackwards = testGroup "Data.Series.Generic.windowing" [ test1, test2 ]
    where
        test1 = testCase "rollingForwards" $ do
            let (series :: Series Vector Int Int) = fromStrictMap $ MS.fromList [(1, 1), (2, 2), (3, 3), (4, 4), (5, 5)]
                expectation = fromStrictMap $ MS.fromList [ (1, 1)
                                                          , (2, 1+2)
                                                          , (3, 2+3)
                                                          , (4, 3+4)
                                                          , (5, 4+5)
                                                          ]
            
            assertEqual mempty expectation $ windowing (\k -> (k-1) `to` k) (Series.sum :: Series Vector Int Int -> Int) series

        test2 = testCase "rollingForwards" $ do
            let (series :: Series Vector Int Int) = fromStrictMap $ MS.fromList [(1, 1), (2, 2), (3, 3), (4, 4), (5, 5)]
                expectation = fromStrictMap $ MS.fromList [ (1, 1)
                                                          , (2, 1+2)
                                                          , (3, 1+2+3)
                                                          , (4, 2+3+4)
                                                          , (5, 3+4+5)
                                                          ]
            
            assertEqual mempty expectation $ windowing (\k -> (k-2) `to` k)  (Series.sum :: Series Vector Int Int -> Int) series


testPropaggregateVsfoldGroups :: TestTree
testPropaggregateVsfoldGroups 
    = testProperty "check that groupBy and testWindowingRollingForwards are equivalent" $ property $ do
        ms <- forAll $ Gen.list (Range.linear 0 100) (Gen.double $ Range.linearFrac (-500) 500) 
        let (xs :: Series Vector Int Double) = Series.fromList (zip [0::Int ..] ms)

        groupBy (`mod` 5) (Series.sum :: Series Vector Int Double -> Double) xs === foldGroups (`mod` 5) (+) xs


testExpanding :: TestTree
testExpanding = testCase "expanding" $ do
    let (xs :: Series Vector Char Int) = Series.fromList $ zip ['a', 'b', 'c', 'd'] [1::Int,2,3,4]
        rs = xs `expanding` Series.sum
        expectation = Series.fromList $ zip ['a', 'b', 'c', 'd'] [1,1+2,1+2+3,1+2+3+4]
    
    assertEqual mempty expectation rs