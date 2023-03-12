module Test.Data.Series.View (tests) where

import qualified Data.Map.Strict      as MS
import           Data.Series          ( Series, fromStrictMap, fromList, from, to, select, reindex, mapIndex )
import qualified Data.Set             as Set
import           Test.Tasty           ( testGroup, TestTree ) 
import           Test.Tasty.HUnit     ( testCase, assertEqual )

tests :: TestTree
tests = testGroup "Data.Series.View" [ testFromTo
                                     , testSelect 
                                     , testReindex
                                     , testMapIndex
                                     ]


testFromTo :: TestTree
testFromTo = testCase "from ... to ..." $ do
    let (series :: Series Char Int) = fromStrictMap $ MS.fromList [('a', 1), ('b', 2), ('c', 3), ('d', 4), ('e', 5)]
        

    let subSeries = series `from` ('b' `to` 'd')
        expectation = fromStrictMap $ MS.fromList [('b', 2), ('c', 3), ('d', 4)]
    assertEqual mempty expectation subSeries


testSelect :: TestTree
testSelect = testCase "select" $ do
    let (series :: Series Char Int) = fromStrictMap $ MS.fromList [('a', 1), ('b', 2), ('c', 3), ('d', 4), ('e', 5)]
        subSeries = series `select` Set.fromList ['a', 'd', 'x']
        expectation = fromStrictMap $ MS.fromList [('a', 1), ('d', 4)]
    
    assertEqual mempty expectation subSeries


testReindex :: TestTree
testReindex = testCase "reindex" $ do
    let (series :: Series Char Int) = fromStrictMap $ MS.fromList [('a', 1), ('b', 2), ('c', 3), ('d', 4), ('e', 5)]
        subSeries = series `reindex` Set.fromList ['a', 'd', 'x']
        expectation = fromStrictMap $ MS.fromList [('a', Just 1), ('d', Just 4), ('x', Nothing)]
    
    assertEqual mempty expectation subSeries


testMapIndex :: TestTree
testMapIndex = testCase "mapIndex" $ do
    let (series :: Series String Int) = fromList [("aa", 1), ("ab", 2), ("bb", 3), ("bc", 4), ("c", 5)]
        subSeries = series `mapIndex` head
        expectation = fromList [('a', 1), ('b', 3), ('c', 5)]
    
    assertEqual mempty expectation subSeries