
module Test.Data.Series.Generic.Zip ( tests ) where


import           Control.Monad        ( forM_ )

import           Data.Maybe           ( fromJust, isNothing )
import           Data.Series.Generic  ( Series(index), mapStrategy
                                      , fromStrictMap, fromList, zipWith, select, at, replace, (|->), (<-|)
                                      )
import qualified Data.Series.Generic  as Series
import qualified Data.Series.Index    as Index 
import           Data.Vector          ( Vector )

import           Hedgehog             ( property, forAll, (===), assert )
import qualified Hedgehog.Gen         as Gen
import qualified Hedgehog.Range       as Range

import           Prelude              hiding ( zipWith )

import           Test.Tasty           ( testGroup, TestTree ) 
import           Test.Tasty.Hedgehog  ( testProperty )
import           Test.Tasty.HUnit     ( testCase, assertEqual )

tests :: TestTree
tests = testGroup "Data.Series.Generic.Zip" [ testZipWith
                                                  , testPropZipWithMatched
                                                  , testPropZipWith
                                                  , testPropReplace
                                                  , testPropReplaceInfix
                                                  , testPropZipWithStrategySkipStrategy
                                                  , testMapStrategy
                                                  ]


testZipWith :: TestTree
testZipWith = testCase "zipWith" $ do
    let (s1 :: Series Vector Char Int) = fromList [('a', 1), ('b', 5)]
        (s2 :: Series Vector Char Int) = fromList [('x', 25), ('b', 10)]
        expectation = fromList [('a', Nothing), ('b', Just 15),  ('x', Nothing)]
    
    assertEqual mempty expectation (zipWith (+) s1 s2)


testPropZipWithMatched :: TestTree
testPropZipWithMatched 
    = testProperty "zipWith when keys all match" $ property $ do
        m1 <- forAll $ Gen.map (Range.linear 0 50) ((,) <$> Gen.alpha <*> Gen.int (Range.linear 0 1000))
        let (xs :: Series Vector Char Int) = fromStrictMap m1
        zipWith (+) xs xs === fmap (Just . (*2)) xs


testPropZipWith :: TestTree
testPropZipWith 
    = testProperty "zipWith when keys all match" $ property $ do
        m1 <- forAll $ Gen.map (Range.linear 0 100) ((,) <$> Gen.string (Range.singleton 2) Gen.alpha <*> Gen.int (Range.linear 0 1000))
        m2 <- forAll $ Gen.map (Range.linear 0 100) ((,) <$> Gen.string (Range.singleton 2) Gen.alpha <*> Gen.int (Range.linear 0 1000))
        let (x1 :: Series Vector String Int) = fromStrictMap m1
            x2 = fromStrictMap m2
            common  = index x1 `Index.intersection` index x2
            symdiff = (index x1 `Index.union` index x2) `Index.difference` common
            comb = zipWith (+) x1 x2

        forM_ common $ \k -> do
            let left  = fromJust $ x1 `at` k
                right = fromJust $ x2 `at` k
            fromJust (comb `at` k) === Just (left + right)
        
        assert $ all isNothing $ Series.values (comb `select` symdiff)


testPropReplace :: TestTree
testPropReplace 
    = testProperty "replace" $ property $ do
        ms <- forAll $ Gen.list (Range.linear 10 100) (Gen.int $ Range.linear (-500) 500) 
        ns <- forAll $ Gen.list (Range.linear 0 10)   (Gen.int $ Range.linear (-500) 500) 
        ixs <- forAll $ Gen.list (Range.singleton $ length ns) (Gen.int $ Range.linear 0 150)
        let (xs :: Series Vector Int Int) = fromList (zip ixs ms)
            ys = fromList (zip [0..] ns)
            rs = ys `replace` xs

        index rs === index xs

        let commonKeys = index xs `Index.intersection` index ys

        (rs `select` commonKeys) === (ys `select` commonKeys)


testPropReplaceInfix :: TestTree
testPropReplaceInfix 
    = testProperty "(|->) and (<-|)" $ property $ do
        ms <- forAll $ Gen.list (Range.linear 10 100) (Gen.int $ Range.linear (-500) 500) 
        ns <- forAll $ Gen.list (Range.linear 0 10)   (Gen.int $ Range.linear (-500) 500) 
        ixs <- forAll $ Gen.list (Range.singleton $ length ns) (Gen.int $ Range.linear 0 150)
        let (xs :: Series Vector Int Int) = fromList (zip ixs ms)
            ys = fromList (zip [0..] ns)
            rs = ys `replace` xs
        
        ys |-> xs === rs 
        ys |-> xs === xs <-| ys 


testPropZipWithStrategySkipStrategy :: TestTree
testPropZipWithStrategySkipStrategy 
    = testProperty "zipWithStrategy f skipStrategy skipStrategy is equivalent to zipWithMatched" $ property $ do
        m1 <- forAll $ Gen.map (Range.linear 0 100) ((,) <$> Gen.string (Range.singleton 2) Gen.alpha <*> Gen.int (Range.linear 0 1000))
        m2 <- forAll $ Gen.map (Range.linear 0 100) ((,) <$> Gen.string (Range.singleton 2) Gen.alpha <*> Gen.int (Range.linear 0 1000))

        let (xs :: Series Vector String Int) = fromStrictMap m1
            ys = fromStrictMap m2

            expectation = Series.zipWithMatched (+) xs ys
        
        expectation === Series.zipWithStrategy (+) Series.skipStrategy Series.skipStrategy xs ys


testMapStrategy :: TestTree
testMapStrategy 
    = testCase "mapStrategy works as expected" $ do
        let (xs :: Series Vector Int Int) = Series.fromList $ zip [0..] [1,2,3,4,5]
            ys =                            Series.fromList $ zip [3..]       [3,4,5]
        
            expected = Series.fromList [ (0, 1+1)
                                       , (1, 2+1)
                                       , (2, 3+1)
                                       , (3, 4+3)
                                       , (4, 5+4)
                                       , (5, 5*2)
                                       ]

        assertEqual mempty expected $ Series.zipWithStrategy (+) (mapStrategy (+1)) (mapStrategy (*2)) xs ys
