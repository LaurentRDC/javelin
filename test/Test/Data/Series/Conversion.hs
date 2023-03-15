
module Test.Data.Series.Conversion (tests) where

import           Data.List            ( nubBy, sortOn )
import qualified Data.Map.Strict      as MS
import qualified Data.Map.Lazy        as ML
import           Data.Series          ( Series, fromStrictMap, toStrictMap, fromLazyMap, toLazyMap, fromList, toList )

import           Hedgehog             ( property, forAll, (===), tripping )
import qualified Hedgehog.Gen         as Gen
import qualified Hedgehog.Range       as Range

import           Test.Tasty           ( testGroup, TestTree ) 
import           Test.Tasty.Hedgehog  ( testProperty )
import           Test.Tasty.HUnit     ( testCase, assertEqual )

tests :: TestTree
tests = testGroup "Data.Series.Conversion" [ testFromStrictMap
                                           , testToStrictMap
                                           , testPropRoundtripConversionWithStrictMap
                                           , testPropRoundtripConversionWithLazyMap
                                           , testPropRoundtripConversionWithList
                                           , testFromLazyMap
                                           , testToLazyMap
                                           ]


testFromStrictMap :: TestTree
testFromStrictMap = testCase "fromStrictMap" $ do
    -- Note the duplicate input at key 'a', which should disappear
    let input = MS.fromList [('b', 2), ('a', 1), ('a', 1)]
        (series :: Series Char Int) = fromStrictMap input
        expectation = fromList [('a', 1), ('b', 2)]
    
    assertEqual mempty series expectation


testToStrictMap :: TestTree
testToStrictMap = testCase "toStrictMap" $ do
    let input = MS.fromList [('b', 2), ('a', 1)]
        (series :: Series Char Int) = fromStrictMap input
    
    assertEqual mempty (toStrictMap series) input


testPropRoundtripConversionWithStrictMap :: TestTree
testPropRoundtripConversionWithStrictMap 
    = testProperty "Roundtrip property with Data.Map.Strict" $ property $ do
        ms <- forAll $ Gen.map (Range.linear 0 50) ((,) <$> Gen.alpha <*> Gen.alpha)
        tripping ms fromStrictMap (Just . toStrictMap)


testPropRoundtripConversionWithLazyMap :: TestTree
testPropRoundtripConversionWithLazyMap 
    = testProperty "Roundtrip property with Data.Map.Lazy" $ property $ do
        ms <- forAll $ Gen.map (Range.linear 0 50) ((,) <$> Gen.alpha <*> Gen.alpha)
        tripping (ML.fromDistinctAscList $ MS.toAscList ms) fromLazyMap (Just . toLazyMap)


testPropRoundtripConversionWithList :: TestTree
testPropRoundtripConversionWithList 
    = testProperty "Roundtrip property with List" $ property $ do
        xs <- forAll $ Gen.list (Range.linear 0 100) ((,) <$> Gen.int (Range.linear (-50) 50) <*> Gen.alpha)

        -- The property below needs some explanation.
        -- In case of conflicting keys, a Series will be biased like a Map. Therefore,
        -- the expected List won't have duplicated (hence the use of nubBy), but the elements which
        -- are kept are in the order of `reverse xs`.
        toList (fromList xs) === sortOn fst (nubBy (\left right -> fst left == fst right) (reverse xs))


testFromLazyMap :: TestTree
testFromLazyMap = testCase "fromLazyMap" $ do
    let input = ML.fromList [('b', 2), ('a', 1)]
        (series :: Series Char Int) = fromLazyMap input
        expectation = fromList [('a', 1), ('b', 2)]
    
    assertEqual mempty series expectation


testToLazyMap :: TestTree
testToLazyMap = testCase "toLazyMap" $ do
    let input = ML.fromList [('b', 2), ('a', 1)]
        (series :: Series Char Int) = fromLazyMap input
    
    assertEqual mempty (toLazyMap series) input