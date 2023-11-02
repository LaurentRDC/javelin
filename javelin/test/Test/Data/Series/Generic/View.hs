module Test.Data.Series.Generic.View (tests) where

import qualified Data.Map.Strict      as MS
import           Data.Series.Generic  ( Series, index, fromStrictMap, fromList, to, from, upto, select
                                      , selectWhere, require, mapIndex, argmax, argmin, )
import qualified Data.Series.Index    as Index
import           Data.Vector          ( Vector )

import           Hedgehog             ( property, forAll, (===), assert )
import qualified Hedgehog.Gen         as Gen
import qualified Hedgehog.Range       as Range

import           Test.Tasty           ( testGroup, TestTree )
import           Test.Tasty.Hedgehog  ( testProperty )
import           Test.Tasty.HUnit     ( testCase, assertEqual )

tests :: TestTree
tests = testGroup "Data.Series.Generic.View" [ testSelectRange
                                             , testSelectUnboundedRange
                                             , testSelectUnboundedRangeEquivalence
                                             , testSelectRangeEmptyRange
                                             , testPropSelectRangeSubseries
                                             , testSelectSet 
                                             , testPropSelectSetSubseries
                                             , testSelectWhere
                                             , testPropRequire
                                             , testMapIndex
                                             , testArgmax
                                             , testArgmin
                                             ]


testSelectRange :: TestTree
testSelectRange = testCase "from ... to ..." $ do
    let (series :: Series Vector Char Int) = fromStrictMap $ MS.fromList [('a', 1), ('b', 2), ('c', 3), ('d', 4), ('e', 5)]
        subSeries = series `select` ('b' `to` 'd')
        expectation = fromStrictMap $ MS.fromList [('b', 2), ('c', 3), ('d', 4)]
    assertEqual mempty expectation subSeries


testSelectUnboundedRange :: TestTree
testSelectUnboundedRange = testCase "from and upto" $ do
    let (series :: Series Vector Char Int) = fromStrictMap $ MS.fromList [('a', 1), ('b', 2), ('c', 3), ('d', 4), ('e', 5)]
        openLeftsubSeries = series `select` from 'b'
        openLeftExpectation = fromStrictMap $ MS.fromList [('b', 2), ('c', 3), ('d', 4), ('e', 5)]
    assertEqual mempty openLeftExpectation openLeftsubSeries

    let openRightsubSeries = series `select` upto 'b'
        openRightExpectation = fromStrictMap $ MS.fromList [('a', 1), ('b', 2)]
    assertEqual mempty openRightExpectation openRightsubSeries


testSelectUnboundedRangeEquivalence :: TestTree
testSelectUnboundedRangeEquivalence 
    = testProperty "Combining unbounded ranges is equivalent to a bounded range" 
    $ property $ do
        m1 <- forAll $ Gen.map (Range.linear 0 50) ((,) <$> Gen.alpha <*> Gen.int (Range.linear 0 1000))
        (b1, b2) <- (,) <$> forAll Gen.alpha <*> forAll Gen.alpha
        let start = min b1 b2
            end = max b1 b2
            (xs :: Series Vector Char Int) = fromStrictMap m1

        (xs `select` start `to` end) === ( (xs `select` from start) `select` upto end)


testPropSelectRangeSubseries :: TestTree
testPropSelectRangeSubseries = testProperty "xs `select` <x> `to` <y> always returns a proper subseries" $ property $ do
        m1 <- forAll $ Gen.map (Range.linear 0 50) ((,) <$> Gen.alpha <*> Gen.int (Range.linear 0 1000))
        start <- forAll Gen.alpha
        end   <- forAll Gen.alpha
        let (xs :: Series Vector Char Int) = fromStrictMap m1
            ys = xs `select` start `to` end
        
        assert $ index xs `Index.contains` index ys


testSelectRangeEmptyRange :: TestTree
testSelectRangeEmptyRange = testCase "from ... to ... on an empty `Range``" $ do
    let (series :: Series Vector Char Int) = fromStrictMap $ MS.fromList [('a', 1), ('b', 2), ('c', 3), ('d', 4), ('e', 5)]
        subSeries = series `select` ('f' `to` 'z')
    assertEqual mempty mempty subSeries


testSelectSet :: TestTree
testSelectSet = testCase "select" $ do
    let (series :: Series Vector Char Int) = fromStrictMap $ MS.fromList [('a', 1), ('b', 2), ('c', 3), ('d', 4), ('e', 5)]
        subSeries = series `select` Index.fromList ['a', 'd', 'x']
        expectation = fromStrictMap $ MS.fromList [('a', 1), ('d', 4)]
    
    assertEqual mempty expectation subSeries


testPropSelectSetSubseries :: TestTree
testPropSelectSetSubseries = testProperty "xs `select` <some set> always returns a proper subseries" $ property $ do
        m1 <- forAll $ Gen.map (Range.linear 0 50) ((,) <$> Gen.alpha <*> Gen.int (Range.linear 0 1000))
        selection <- forAll $ Gen.set (Range.linear 0 10) Gen.alpha
        let (xs :: Series Vector Char Int) = fromStrictMap m1
            ys = xs `select` selection
        
        assert $ index xs `Index.contains` index ys


testSelectWhere :: TestTree
testSelectWhere = testCase "selectWhere" $ do
    let (series :: Series Vector Char Int) = fromStrictMap $ MS.fromList [('a', 1), ('b', 2), ('c', 3), ('d', 4), ('e', 5)]
        subSeries = series `selectWhere` fmap (>3) series 
        expectation = fromStrictMap $ MS.fromList [('d', 4), ('e', 5)]
    
    assertEqual mempty expectation subSeries


testPropRequire :: TestTree
testPropRequire = testProperty "require always returns a series with the expected index" $ property $ do
    m1 <- forAll $ Gen.map (Range.linear 0 50) ((,) <$> Gen.int (Range.linear 0 1000) <*> Gen.int (Range.linear 0 1000))
    ss <- forAll $ Gen.set (Range.linear 0 100) (Gen.int (Range.linear (-100) 100))
    
    let (xs :: Series Vector Int Int) = fromStrictMap m1
        ix = Index.fromSet ss
    index (xs `require` ix) === ix 


testMapIndex :: TestTree
testMapIndex = testCase "mapIndex" $ do
    let (series :: Series Vector String Int) = fromList [("aa", 1), ("ab", 2), ("bb", 3), ("bc", 4), ("c", 5)]
        subSeries = series `mapIndex` take 1
        expectation = fromList [("a", 1), ("b", 3), ("c", 5)]
    
    assertEqual mempty expectation subSeries


testArgmax :: TestTree
testArgmax = testCase "argmax" $ do
    let (series :: Series Vector String Int) = fromList [("aa", 1), ("ab", 2), ("bb", 10), ("bc", 4), ("c", 5)]
        expectation = Just "bb"
    
    assertEqual mempty expectation (argmax series)

testArgmin :: TestTree
testArgmin = testCase "argmin" $ do
    let (series :: Series Vector String Int) = fromList [("aa", 1), ("ab", 2), ("bb", -10), ("bc", 4), ("c", 5)]
        expectation = Just "bb"
    
    assertEqual mempty expectation (argmin series)