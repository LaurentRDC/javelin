
module Test.Data.Series.Windowing (tests) where

import           Data.Series          ( fromList, to, windows )

import           Test.Tasty           ( testGroup, TestTree ) 
import           Test.Tasty.HUnit     ( testCase, assertEqual )

tests :: TestTree
tests = testGroup "Data.Series.Windowing" [ testWindows ]


testWindows :: TestTree
testWindows = testCase "windows" $ do
    let xs = fromList $ zip [0::Int ..] ['a', 'b', 'c']
        ws = windows (\i -> i `to` (i+1)) xs
        expectation = [ fromList [(0, 'a'), (1, 'b')]
                      , fromList [(1, 'b'), (2, 'c')]
                      , fromList [(2, 'c')]
                      ]
    assertEqual mempty expectation ws

