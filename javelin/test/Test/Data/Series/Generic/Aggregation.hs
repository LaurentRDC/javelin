
module Test.Data.Series.Generic.Aggregation (tests) where

import qualified Data.Map.Strict      as MS
import qualified Data.Series.Generic  as Series
import           Data.Series.Generic  ( Series, fromStrictMap, groupBy, aggregateWith, foldGroupsWith)
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
                                                    , testPropAggregateWithVsFoldGroupsWith
                                                    ]


testGroupBy :: TestTree
testGroupBy = testGroup "Data.Series.groupBy" [ testGroupBy1, testGroupBy2 ]
    where
        testGroupBy1 = testCase "groupBy" $ do
            let (series :: Series Vector String Int) = fromStrictMap $ MS.fromList [("aa", 1), ("ab", 2), ("c", 3), ("dc", 4), ("ae", 5)]
                expectation = fromStrictMap $ MS.fromList [('a', 1+2+5), ('c', 3), ('d', 4)]
            
            assertEqual mempty expectation $ (series `groupBy` head) `aggregateWith` Series.sum

        testGroupBy2 = testCase "groupBy" $ do
            let (series :: Series Vector Int Int) = fromStrictMap $ MS.fromList $ zip [0,1,2,3] [0,1,2,3]
                expectation = fromStrictMap $ MS.fromList [(True, 0+2), (False, 1+3)]
            
            assertEqual mempty expectation $ (series `groupBy` even) `aggregateWith` Series.sum


testPropAggregateWithVsFoldGroupsWith :: TestTree
testPropAggregateWithVsFoldGroupsWith 
    = testProperty "check that aggregateWith and foldGroupsWith are equivalent" $ property $ do
        ms <- forAll $ Gen.list (Range.linear 0 100) (Gen.double $ Range.linearFrac (-500) 500) 
        let (xs :: Series Vector Int Double) = Series.fromList (zip [0::Int ..] ms)

        (xs `groupBy` (`mod` 5) `aggregateWith` Series.sum) === (xs `groupBy` (`mod` 5) `foldGroupsWith` (+))