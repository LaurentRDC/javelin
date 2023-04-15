
module Test.Data.Series.Generic.Windowing (tests) where

import           Data.Series.Generic  ( Series, fromList, to, windows, iwindows, expanding, irolling )
import qualified Data.Series.Generic  as Series
import           Data.Vector          ( Vector )

import           Test.Tasty           ( testGroup, TestTree ) 
import           Test.Tasty.HUnit     ( testCase, assertEqual )

tests :: TestTree
tests = testGroup "Data.Series.Generic.Windowing" [ testWindows
                                                  , testIwindows
                                                  , testExpanding 
                                                  , testIrolling
                                                  ]


testWindows :: TestTree
testWindows = testCase "windows" $ do
    let (xs :: Series Vector Int Char) = fromList $ zip [0::Int ..] ['a', 'b', 'c']
        ws = windows (\i -> i `to` (i+1)) xs
        expectation = [ fromList [(0, 'a'), (1, 'b')]
                      , fromList [(1, 'b'), (2, 'c')]
                      , fromList [(2, 'c')]
                      ]
    assertEqual mempty expectation ws


testIwindows :: TestTree
testIwindows = testCase "iwindows" $ do
    let (xs :: Series Vector Int Char) = fromList $ zip [0::Int ..] ['a', 'b', 'c']
        ws = iwindows (\i -> i `to` (i+1)) xs
        expectation = [ fromList [(0, 'a'), (1, 'b')]
                      , fromList [(1, 'b'), (2, 'c')]
                      , fromList [(2, 'c')]
                      ]
    assertEqual mempty expectation ws


testExpanding :: TestTree
testExpanding = testCase "expanding" $ do
    let (xs :: Series Vector Char Int) = fromList $ zip ['a', 'b', 'c', 'd'] [1::Int,2,3,4]
        rs = xs `expanding` Series.sum
        expectation = fromList $ zip ['a', 'b', 'c', 'd'] [1,1+2,1+2+3,1+2+3+4]
    
    assertEqual mempty expectation rs


testIrolling :: TestTree
testIrolling = testCase "irolling" $ do
    let (xs :: Series Vector Char Int) = fromList $ zip ['a', 'b', 'c', 'd'] [1::Int,2,3,4]
        rs = irolling 2 (-1) Series.sum xs
        expectation = fromList [ ('a', -1)
                               , ('b', 1+2)
                               , ('c', 2+3)
                               , ('d', 3+4)
                               ]
    
    assertEqual mempty expectation rs