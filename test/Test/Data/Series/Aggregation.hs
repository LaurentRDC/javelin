
module Test.Data.Series.Aggregation (tests) where

import qualified Data.Map.Strict      as MS
import           Data.Series          ( Series, fromStrictMap, groupBy, aggregateWith)

import           Prelude              hiding ( zipWith )

import           Test.Tasty           ( testGroup, TestTree ) 
import           Test.Tasty.HUnit     ( testCase, assertEqual )

tests :: TestTree
tests = testGroup "Data.Series.Aggregation" [ testGroupBy
                                            ]


testGroupBy :: TestTree
testGroupBy = testCase "groupBy" $ do
    let (series :: Series String Int) = fromStrictMap $ MS.fromList [("aa", 1), ("ab", 2), ("c", 3), ("dc", 4), ("ae", 5)]
        expectation = fromStrictMap $ MS.fromList [('a', 1+2+5), ('c', 3), ('d', 4)]
    
    assertEqual mempty expectation $ (series `groupBy` head) `aggregateWith` sum