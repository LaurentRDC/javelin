
module Test.Data.Series.Generic.Broadcast ( tests ) where


import           Control.Monad        ( forM_ )

import           Data.Maybe           ( fromJust, isNothing )
import           Data.Series.Generic  ( Series(index)
                                      , fromStrictMap, fromList, zipWith, select, at
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
tests = testGroup "Data.Series.Generic.Broadcast" [ testZipWith
                                                  , testPropZipWithMatched
                                                  , testPropZipWith
                                                  , testPropZipWithStrategySkipStrategy
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


testPropZipWithStrategySkipStrategy :: TestTree
testPropZipWithStrategySkipStrategy 
    = testProperty "zipWithStrategy f skipStrategy skipStrategy is equivalent to zipWithMatched" $ property $ do
        m1 <- forAll $ Gen.map (Range.linear 0 100) ((,) <$> Gen.string (Range.singleton 2) Gen.alpha <*> Gen.int (Range.linear 0 1000))
        m2 <- forAll $ Gen.map (Range.linear 0 100) ((,) <$> Gen.string (Range.singleton 2) Gen.alpha <*> Gen.int (Range.linear 0 1000))

        let (xs :: Series Vector String Int) = fromStrictMap m1
            ys = fromStrictMap m2

            expectation = Series.zipWithMatched (+) xs ys
        
        expectation === Series.zipWithStrategy (+) Series.skipStrategy Series.skipStrategy xs ys

