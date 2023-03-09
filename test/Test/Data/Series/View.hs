module Test.Data.Series.View (tests) where

import qualified Data.Map.Strict      as MS
import           Data.Series          ( Series, fromStrictMap, from, to )
import           Test.Tasty           ( testGroup, TestTree ) 
import           Test.Tasty.HUnit     ( testCase, assertEqual )

tests :: TestTree
tests = testGroup "Data.Series.View" [ testFromTo ]


testFromTo :: TestTree
testFromTo = testCase "from ... to ..." $ do
    let (series :: Series Char Int) = fromStrictMap $ MS.fromList [('a', 1), ('b', 2), ('c', 3), ('d', 4), ('e', 5)]
        

    let subSeries = series `from` ('b' `to` 'd')
        expectation = fromStrictMap $ MS.fromList [('b', 2), ('c', 3), ('d', 4)]
    assertEqual mempty expectation subSeries