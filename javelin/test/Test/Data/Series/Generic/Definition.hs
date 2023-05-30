
module Test.Data.Series.Generic.Definition (tests) where

import           Data.List            ( nubBy, sortOn )
import qualified Data.Map.Strict      as MS
import qualified Data.Map.Lazy        as ML
import           Data.Series.Generic  ( Series, Occ, fromStrictMap, toStrictMap, fromLazyMap, toLazyMap, fromList, toList )
import qualified Data.Series.Generic  as Series
import           Data.Vector          ( Vector )

import           Hedgehog             ( property, forAll, (===), tripping )
import qualified Hedgehog.Gen         as Gen
import qualified Hedgehog.Range       as Range

import           Test.Tasty           ( testGroup, TestTree ) 
import           Test.Tasty.Hedgehog  ( testProperty )
import           Test.Tasty.HUnit     ( testCase, assertEqual )

tests :: TestTree
tests = testGroup "Data.Series.Generic.Definition" [ testMappend
                                                   , testPropMappendLikeMap
                                                   , testPropShow
                                                   , testFromStrictMap
                                                   , testToStrictMap
                                                   , testPropRoundtripConversionWithStrictMap
                                                   , testPropRoundtripConversionWithLazyMap
                                                   , testPropRoundtripConversionWithList
                                                   , testPropFromListDuplicatesNeverDrops
                                                   , testFromLazyMap
                                                   , testToLazyMap
                                                   , testTakeWhile
                                                   , testDropWhile
                                                   ]


testMappend :: TestTree
testMappend = testCase "(<>)" $ do
    let (s1 :: Series Vector Char Int) = fromList [('a', 1), ('b', 5)]
        (s2 :: Series Vector Char Int) = fromList [('b', 10), ('x', 25)]
        expectation = fromList [('a', 1), ('b', 5),  ('x', 25)]
    
    assertEqual mempty expectation (s1 <> s2)


testPropMappendLikeMap :: TestTree
testPropMappendLikeMap 
    = testProperty "Mappend property similar to Data.Map.Strict" $ property $ do
        m1 <- forAll $ Gen.map (Range.linear 0 50) ((,) <$> Gen.int (Range.linear 0 1000)   <*> Gen.alpha)
        m2 <- forAll $ Gen.map (Range.linear 0 50) ((,) <$> Gen.int (Range.linear 500 1500) <*> Gen.alpha)

        (fromStrictMap :: MS.Map Int Char -> Series Vector Int Char) (m1 <> m2) === fromStrictMap m1 <> fromStrictMap m2


testPropShow :: TestTree
testPropShow
    = testProperty "Show is never too long" $ property $ do
        m1 <- forAll $ Gen.map (Range.linear 0 50) ((,) <$> Gen.int (Range.linear 0 1000)   <*> Gen.alpha)

        let (xs :: Series Vector Int Char) = fromStrictMap m1
            ls = lines $ show xs
        if Series.length xs > 6
            then length ls === 2 + 6 + 1
            else length ls === 2 + Series.length xs


testFromStrictMap :: TestTree
testFromStrictMap = testCase "fromStrictMap" $ do
    -- Note the duplicate input at key 'a', which should disappear
    let input = MS.fromList [('b', 2), ('a', 1), ('a', 1)]
        (series :: Series Vector Char Int) = fromStrictMap input
        expectation = fromList [('a', 1), ('b', 2)]
    
    assertEqual mempty series expectation


testToStrictMap :: TestTree
testToStrictMap = testCase "toStrictMap" $ do
    let input = MS.fromList [('b', 2), ('a', 1)]
        (series :: Series Vector Char Int) = fromStrictMap input
    
    assertEqual mempty (toStrictMap series) input


testPropRoundtripConversionWithStrictMap :: TestTree
testPropRoundtripConversionWithStrictMap 
    = testProperty "Roundtrip property with Data.Map.Strict" $ property $ do
        ms <- forAll $ Gen.map (Range.linear 0 50) ((,) <$> Gen.alpha <*> Gen.alpha)
        tripping ms (fromStrictMap :: MS.Map Char Char -> Series Vector Char Char) (Just . toStrictMap)


testPropRoundtripConversionWithLazyMap :: TestTree
testPropRoundtripConversionWithLazyMap 
    = testProperty "Roundtrip property with Data.Map.Lazy" $ property $ do
        ms <- forAll $ Gen.map (Range.linear 0 50) ((,) <$> Gen.alpha <*> Gen.alpha)
        tripping (ML.fromDistinctAscList $ MS.toAscList ms) (fromLazyMap :: MS.Map Char Char -> Series Vector Char Char) (Just . toLazyMap)


testPropRoundtripConversionWithList :: TestTree
testPropRoundtripConversionWithList 
    = testProperty "Roundtrip property with List" $ property $ do
        xs <- forAll $ Gen.list (Range.linear 0 100) ((,) <$> Gen.int (Range.linear (-50) 50) <*> Gen.alpha)

        -- The property below needs some explanation.
        -- In case of conflicting keys, a Series will be biased like a Map. Therefore,
        -- the expected List won't have duplicated (hence the use of nubBy), but the elements which
        -- are kept are in the order of `reverse xs`.
        (toList :: Series Vector Int Char -> [(Int, Char)] ) (fromList xs) === sortOn fst (nubBy (\left right -> fst left == fst right) (reverse xs))


testPropFromListDuplicatesNeverDrops :: TestTree
testPropFromListDuplicatesNeverDrops
    = testProperty "fromListDuplicates never drops elements" $ property $ do
        xs <- forAll $ Gen.list (Range.linear 0 100) ((,) <$> Gen.int (Range.linear (-10) 10) <*> Gen.alpha)
        Series.length (Series.fromListDuplicates xs :: Series Vector (Int, Occ) Char) === length xs


testFromLazyMap :: TestTree
testFromLazyMap = testCase "fromLazyMap" $ do
    let input = ML.fromList [('b', 2), ('a', 1)]
        (series :: Series Vector Char Int) = fromLazyMap input
        expectation = fromList [('a', 1), ('b', 2)]
    
    assertEqual mempty series expectation


testToLazyMap :: TestTree
testToLazyMap = testCase "toLazyMap" $ do
    let input = ML.fromList [('b', 2), ('a', 1)]
        (series :: Series Vector Char Int) = fromLazyMap input
    
    assertEqual mempty (toLazyMap series) input


testTakeWhile :: TestTree
testTakeWhile = testProperty "takeWhile behaves like lists" $ property $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.linear (-50) 50))
    let (ys :: Series Vector Int Int) = Series.fromList $ zip [0..] xs

    n  <- forAll $ Gen.int  (Range.linear 1 10)
    Series.takeWhile (\v -> v `mod` n == 0) ys === Series.fromList (takeWhile (\(_, v) -> v `mod` n == 0) $ Series.toList ys)


testDropWhile :: TestTree
testDropWhile = testProperty "dropWhile behaves like lists" $ property $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.linear (-50) 50))
    let (ys :: Series Vector Int Int) = Series.fromList $ zip [0..] xs

    n  <- forAll $ Gen.int  (Range.linear 1 10)
    Series.dropWhile (\v -> v `mod` n /= 0) ys === Series.fromList (dropWhile (\(_, v) -> v `mod` n /= 0) $ Series.toList ys)