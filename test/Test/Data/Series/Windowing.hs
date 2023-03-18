
module Test.Data.Series.Windowing (tests) where

import           Data.Series          ( fromList, to, windows, iwindows, expanding, irolling )

import           Test.Tasty           ( testGroup, TestTree ) 
import           Test.Tasty.HUnit     ( testCase, assertEqual )

tests :: TestTree
tests = testGroup "Data.Series.Windowing" [ testWindows
                                          , testIwindows
                                          , testExpanding 
                                          , testIrolling
                                          ]


testWindows :: TestTree
testWindows = testCase "windows" $ do
    let xs = fromList $ zip [0::Int ..] ['a', 'b', 'c']
        ws = windows (\i -> i `to` (i+1)) xs
        expectation = [ fromList [(0, 'a'), (1, 'b')]
                      , fromList [(1, 'b'), (2, 'c')]
                      , fromList [(2, 'c')]
                      ]
    assertEqual mempty expectation ws


testIwindows :: TestTree
testIwindows = testCase "iwindows" $ do
    let xs = fromList $ zip [0::Int ..] ['a', 'b', 'c']
        ws = iwindows (\i -> i `to` (i+1)) xs
        expectation = [ fromList [(0, 'a'), (1, 'b')]
                      , fromList [(1, 'b'), (2, 'c')]
                      , fromList [(2, 'c')]
                      ]
    assertEqual mempty expectation ws


testExpanding :: TestTree
testExpanding = testCase "expanding" $ do
    let xs = fromList $ zip ['a', 'b', 'c', 'd'] [1::Int,2,3,4]
        rs = xs `expanding` sum
        expectation = fromList $ zip ['a', 'b', 'c', 'd'] [1,1+2,1+2+3,1+2+3+4]
    
    assertEqual mempty expectation rs


testIrolling :: TestTree
testIrolling = testCase "irolling" $ do
    let xs = fromList $ zip ['a', 'b', 'c', 'd'] [1::Int,2,3,4]
        rs = irolling 2 (-1) sum xs
        expectation = fromList [ ('a', -1)
                               , ('b', 1+2)
                               , ('c', 2+3)
                               , ('d', 3+4)
                               ]
    
    assertEqual mempty expectation rs