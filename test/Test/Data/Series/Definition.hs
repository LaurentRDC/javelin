
module Test.Data.Series.Definition (tests) where

import           Data.Series          ( Series, fromList )
import           Test.Tasty           ( testGroup, TestTree ) 
import           Test.Tasty.HUnit     ( testCase, assertEqual )

tests :: TestTree
tests = testGroup "Data.Series.Definition" [ testMappend ]


testMappend :: TestTree
testMappend = testCase "(<>)" $ do
    let (s1 :: Series Char Int) = fromList [('a', 1), ('b', 5)]
        (s2 :: Series Char Int) = fromList [('b', 10), ('x', 25)]
        expectation = fromList [('a', 1), ('b', 5),  ('x', 25)]
    
    assertEqual mempty expectation (s1 <> s2)