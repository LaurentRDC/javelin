
module Test.Data.Series.Generic.Aggregation (tests) where

import qualified Data.IntMap.Strict   as IS
import qualified Data.Map.Strict      as MS
import qualified Data.Series.Generic  as Series
import           Data.Series.Generic  ( Series, fromStrictMap, groupBy, aggregateWith, foldWith, windowing, to, expanding)
import           Data.Vector          ( Vector )
import qualified Data.Vector          as Vector

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
                                                    , testPropAggregateVsfoldWith
                                                    , testExpanding
                                                    ]


testGroupBy :: TestTree
testGroupBy = testGroup "Data.Series.Generic.groupBy" 
            [ testGroupBy1
            , testGroupBy2
            , testGroupBy3
            , testGroupBy4
            , testGroupBy5 
            ]
    where
        testGroupBy1 = testCase "groupBy" $ do
            let (series :: Series Vector String Int) = fromStrictMap $ MS.fromList [("aa", 1), ("ab", 2), ("c", 3), ("dc", 4), ("ae", 5)]
                expectation = fromStrictMap $ MS.fromList [(1, 3), (2, 1+2+4+5)]
            
            assertEqual mempty expectation $ series `groupBy` length `aggregateWith` (Series.sum :: Series Vector String Int -> Int)

        testGroupBy2 = testCase "groupBy" $ do
            let (series :: Series Vector Int Int) = fromStrictMap $ MS.fromList $ zip [0,1,2,3] [0,1,2,3]
                expectation = fromStrictMap $ MS.fromList [(True, 0+2), (False, 1+3)]
            
            assertEqual mempty expectation $ series `groupBy` even `aggregateWith` (Series.sum :: Series Vector Int Int -> Int)

        -- The following example resulted in an exception
        -- when the implementation of `aggregateWith` didn't aggregate keys in the
        -- right order
        testGroupBy3 = testCase "groupBy" $ do
            let (series :: Series Vector (Int, Int) Int) = fromStrictMap $ MS.fromList [ ((0, 0), 1), ((0, 1), 2) ]
                expectation = fromStrictMap $ MS.fromList [(0, IS.fromList [(0, 1), (1, 2)])]
            
            assertEqual mempty expectation $ series `groupBy` fst `aggregateWith` (IS.fromList . Series.toList . flip Series.mapIndex snd)

        -- The following example resulted in an exception
        -- when the implementation of `aggregateWith` didn't aggregate keys in the
        -- right order
        testGroupBy4 = testCase "groupBy" $ do
            let (series :: Series Vector (Int, Int) Int) = fromStrictMap $ MS.fromList $ zip (zip [0,1,2,3,4,5] [1,1,2,2,3,3]) [2,1,2,1,2,1]
                expectation = fromStrictMap $ MS.fromList [ (1, Vector.fromList [2,1])
                                                          , (2, Vector.fromList [2,1])
                                                          , (3, Vector.fromList [2,1])
                                                          ]
            
            assertEqual mempty expectation $ series `groupBy` snd `aggregateWith` (Series.values)
        
        -- The following test ensures that the order of folding in `foldWith` is intuitive.
        testGroupBy5 = testCase "groupBy" $ do
            let (series :: Series Vector Int [Int]) = fromStrictMap $ MS.fromList $ zip [0,1,2,3,4,5] [[0],[1],[2],[3],[4],[5]]
                expectation = fromStrictMap $ MS.fromList [ (True, [0, 2, 4])
                                                          , (False, [1, 3, 5])
                                                          ]
            
            assertEqual mempty expectation $ series `groupBy` isEven `foldWith` (++)
                where
                    isEven i = i `mod` 2 == 0


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


testPropAggregateVsfoldWith :: TestTree
testPropAggregateVsfoldWith 
    = testProperty "check that groupBy and testWindowingRollingForwards are equivalent" $ property $ do
        ms <- forAll $ Gen.list (Range.linear 0 100) (Gen.int $ Range.linear (-500) 500) 
        let (xs :: Series Vector Int Int) = Series.fromList (zip [0::Int ..] ms)

        xs `groupBy` (`mod` 5) `aggregateWith` (Series.sum :: Series Vector Int Int -> Int) === xs `groupBy` (`mod` 5) `foldWith` (+)


testExpanding :: TestTree
testExpanding = testCase "expanding" $ do
    let (xs :: Series Vector Char Int) = Series.fromList $ zip ['a', 'b', 'c', 'd'] [1::Int,2,3,4]
        rs = xs `expanding` Series.sum
        expectation = Series.fromList $ zip ['a', 'b', 'c', 'd'] [1,1+2,1+2+3,1+2+3+4]
    
    assertEqual mempty expectation rs