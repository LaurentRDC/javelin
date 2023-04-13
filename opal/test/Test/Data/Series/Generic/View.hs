module Test.Data.Series.Generic.View (tests) where

import qualified Data.Map.Strict      as MS
import           Data.Series.Generic  ( Series, fromStrictMap, fromList, to, select, selectWhere, reindex, mapIndex )
import qualified Data.Series.Index    as Index
import           Data.Vector          ( Vector )
import           Test.Tasty           ( testGroup, TestTree ) 
import           Test.Tasty.HUnit     ( testCase, assertEqual )

tests :: TestTree
tests = testGroup "Data.Series.Generic.View" [ testSelectRange
                                             , testSelectSet 
                                             , testSelectWhere
                                             , testReindex
                                             , testMapIndex
                                             ]


testSelectRange :: TestTree
testSelectRange = testCase "from ... to ..." $ do
    let (series :: Series Vector Char Int) = fromStrictMap $ MS.fromList [('a', 1), ('b', 2), ('c', 3), ('d', 4), ('e', 5)]
        subSeries = series `select` ('b' `to` 'd')
        expectation = fromStrictMap $ MS.fromList [('b', 2), ('c', 3), ('d', 4)]
    assertEqual mempty expectation subSeries


testSelectSet :: TestTree
testSelectSet = testCase "select" $ do
    let (series :: Series Vector Char Int) = fromStrictMap $ MS.fromList [('a', 1), ('b', 2), ('c', 3), ('d', 4), ('e', 5)]
        subSeries = series `select` Index.fromList ['a', 'd', 'x']
        expectation = fromStrictMap $ MS.fromList [('a', 1), ('d', 4)]
    
    assertEqual mempty expectation subSeries


testSelectWhere :: TestTree
testSelectWhere = testCase "selectWhere" $ do
    let (series :: Series Vector Char Int) = fromStrictMap $ MS.fromList [('a', 1), ('b', 2), ('c', 3), ('d', 4), ('e', 5)]
        subSeries = series `selectWhere` fmap (>3) series 
        expectation = fromStrictMap $ MS.fromList [('d', 4), ('e', 5)]
    
    assertEqual mempty expectation subSeries


testReindex :: TestTree
testReindex = testCase "reindex" $ do
    let (series :: Series Vector Char Int) = fromStrictMap $ MS.fromList [('a', 1), ('b', 2), ('c', 3), ('d', 4), ('e', 5)]
        subSeries = series `reindex` Index.fromList ['a', 'd', 'x']
        expectation = fromStrictMap $ MS.fromList [('a', Just 1), ('d', Just 4), ('x', Nothing)]
    
    assertEqual mempty expectation subSeries


testMapIndex :: TestTree
testMapIndex = testCase "mapIndex" $ do
    let (series :: Series Vector String Int) = fromList [("aa", 1), ("ab", 2), ("bb", 3), ("bc", 4), ("c", 5)]
        subSeries = series `mapIndex` head
        expectation = fromList [('a', 1), ('b', 3), ('c', 5)]
    
    assertEqual mempty expectation subSeries