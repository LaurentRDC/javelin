
module Test.Data.Series.Conversion (tests) where

import qualified Data.Map.Strict      as MS
import qualified Data.Map.Lazy        as ML
import           Data.Series          ( Series, fromStrictMap, toStrictMap, fromLazyMap, toLazyMap, fromAscList )

import           Test.Tasty           ( testGroup, TestTree ) 
import           Test.Tasty.HUnit     ( testCase, assertEqual )

tests :: TestTree
tests = testGroup "Data.Series.Conversion" [ testFromStrictMap
                                           , testToStrictMap
                                           , testFromLazyMap
                                           , testToLazyMap
                                           ]


testFromStrictMap :: TestTree
testFromStrictMap = testCase "fromStrictMap" $ do
    let input = MS.fromList [('b', 2), ('a', 1)]
        (series :: Series Char Int) = fromStrictMap input
        expectation = fromAscList [('a', 1), ('b', 2)]
    
    assertEqual mempty series expectation


testToStrictMap :: TestTree
testToStrictMap = testCase "toStrictMap" $ do
    let input = MS.fromList [('b', 2), ('a', 1)]
        (series :: Series Char Int) = fromStrictMap input
    
    assertEqual mempty (toStrictMap series) input


testFromLazyMap :: TestTree
testFromLazyMap = testCase "fromLazyMap" $ do
    let input = ML.fromList [('b', 2), ('a', 1)]
        (series :: Series Char Int) = fromLazyMap input
        expectation = fromAscList [('a', 1), ('b', 2)]
    
    assertEqual mempty series expectation


testToLazyMap :: TestTree
testToLazyMap = testCase "toLazyMap" $ do
    let input = ML.fromList [('b', 2), ('a', 1)]
        (series :: Series Char Int) = fromLazyMap input
    
    assertEqual mempty (toLazyMap series) input