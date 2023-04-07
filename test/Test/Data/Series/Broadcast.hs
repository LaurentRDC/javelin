
module Test.Data.Series.Broadcast ( tests ) where


import           Control.Monad        ( forM_ )

import           Data.Maybe           ( fromJust, isNothing )
import           Data.Series          ( Series(index)
                                      , (+:), (-:), (*:), (/:), (==:), (/=:)
                                      , (+|), (-|), (*|), (/|), (==|), (/=|)
                                      , fromStrictMap, fromList, zipWith, select, at
                                      )
import qualified Data.Series          as Series
import qualified Data.Series.Index    as Index 

import           Hedgehog             ( property, forAll, (===), assert )
import qualified Hedgehog.Gen         as Gen
import qualified Hedgehog.Range       as Range

import           Prelude              hiding ( zipWith )

import           Test.Tasty           ( testGroup, TestTree ) 
import           Test.Tasty.Hedgehog  ( testProperty )
import           Test.Tasty.HUnit     ( testCase, assertEqual )
import           Test.Utils           ( approx )

tests :: TestTree
tests = testGroup "Data.Series.Broadcast" [ testZipWith
                                          , testPropZipWithMatched
                                          , testPropZipWith
                                          , testPropZipWithStrategySkipStrategy
                                          , testPropPlusMaybe
                                          , testPropMinusMaybe
                                          , testPropMultMaybe
                                          , testPropDivMaybe
                                          , testPropEqMaybe
                                          , testPropNotEqMaybe
                                          , testPropPlusMatched
                                          , testPropMinusMatched
                                          , testPropMultMatched
                                          , testPropDivMatched
                                          , testPropEqMatched
                                          , testPropNotEqMatched
                                          ]


testZipWith :: TestTree
testZipWith = testCase "zipWith" $ do
    let (s1 :: Series Char Int) = fromList [('a', 1), ('b', 5)]
        (s2 :: Series Char Int) = fromList [('x', 25), ('b', 10)]
        expectation = fromList [('a', Nothing), ('b', Just 15),  ('x', Nothing)]
    
    assertEqual mempty expectation (zipWith (+) s1 s2)


testPropZipWithMatched :: TestTree
testPropZipWithMatched 
    = testProperty "zipWith when keys all match" $ property $ do
        m1 <- forAll $ Gen.map (Range.linear 0 50) ((,) <$> Gen.alpha <*> Gen.int (Range.linear 0 1000))
        let xs = fromStrictMap m1
        zipWith (+) xs xs === fmap (Just . (*2)) xs


testPropZipWith :: TestTree
testPropZipWith 
    = testProperty "zipWith when keys all match" $ property $ do
        m1 <- forAll $ Gen.map (Range.linear 0 100) ((,) <$> Gen.text (Range.singleton 2) Gen.alpha <*> Gen.int (Range.linear 0 1000))
        m2 <- forAll $ Gen.map (Range.linear 0 100) ((,) <$> Gen.text (Range.singleton 2) Gen.alpha <*> Gen.int (Range.linear 0 1000))
        let x1 = fromStrictMap m1
            x2 = fromStrictMap m2
            common  = index x1 `Index.intersection` index x2
            symdiff = (index x1 `Index.union` index x2) `Index.difference` common
            comb = zipWith (+) x1 x2

        forM_ common $ \k -> do
            let left  = fromJust $ x1 `at` k
                right = fromJust $ x2 `at` k
            fromJust (comb `at` k) === Just (left + right)
        
        assert $ all isNothing (comb `select` symdiff)


testPropZipWithStrategySkipStrategy :: TestTree
testPropZipWithStrategySkipStrategy 
    = testProperty "zipWithStrategy f skipStrategy skipStrategy is equivalent to zipWithMatched" $ property $ do
        m1 <- forAll $ Gen.map (Range.linear 0 100) ((,) <$> Gen.text (Range.singleton 2) Gen.alpha <*> Gen.int (Range.linear 0 1000))
        m2 <- forAll $ Gen.map (Range.linear 0 100) ((,) <$> Gen.text (Range.singleton 2) Gen.alpha <*> Gen.int (Range.linear 0 1000))

        let xs = fromStrictMap m1
            ys = fromStrictMap m2

            expectation = Series.zipWithMatched (+) xs ys
        
        expectation === Series.zipWithStrategy (+) Series.skipStrategy Series.skipStrategy xs ys


testPropPlusMaybe :: TestTree
testPropPlusMaybe
    = testProperty "Broadcastable addition with holes (+:)" $ property $ do
        m1 <- forAll $ Gen.map (Range.linear 0 100) ((,) <$> Gen.text (Range.singleton 2) Gen.alpha <*> Gen.int (Range.linear 0 1000))
        c  <- forAll $ Gen.int (Range.linear 0 100)

        let xs = fromStrictMap m1
            expectation = fmap (+c) xs
        expectation === (xs +: c)
        expectation === (c +: xs)
        fmap (Just . (*2)) xs === xs +: xs


testPropMinusMaybe :: TestTree
testPropMinusMaybe
    = testProperty "Broadcastable subtraction with holes (-:)" $ property $ do
        m1 <- forAll $ Gen.map (Range.linear 0 100) ((,) <$> Gen.text (Range.singleton 2) Gen.alpha <*> Gen.int (Range.linear 0 1000))
        c  <- forAll $ Gen.int (Range.linear 0 100)

        let xs = fromStrictMap m1
        fmap (\x -> x - c) xs === (xs -: c)
        fmap (c -) xs         === (c -: xs)
        fmap (const (Just (0 :: Int))) xs === xs -: xs


testPropMultMaybe :: TestTree
testPropMultMaybe
    = testProperty "Broadcastable multiplication with holes (*:)" $ property $ do
        m1 <- forAll $ Gen.map (Range.linear 0 100) ((,) <$> Gen.text (Range.singleton 2) Gen.alpha <*> Gen.int (Range.linear 0 1000))
        c  <- forAll $ Gen.int (Range.linear 0 100)

        let xs = fromStrictMap m1
        fmap (*c) xs === (xs *: c)
        fmap (*c) xs === (c *: xs)


testPropDivMaybe :: TestTree
testPropDivMaybe
    = testProperty "Broadcastable division with holes (/:)" $ property $ do
        m1 <- forAll $ Gen.map (Range.linear 0 100) ((,) <$> Gen.text (Range.singleton 2) Gen.alpha <*> Gen.double (Range.linearFrac (-500) 500))
        c  <- forAll $ Gen.double (Range.linearFrac (-500) 500)

        let xs = fromStrictMap m1
        forM_ (index xs) $ \k -> do
            fromJust (fmap (/c) xs `at` k) `approx` fromJust ( (xs /: c) `at` k)
            fromJust (fmap (c/) xs `at` k) `approx` fromJust (  (c /: xs) `at` k)

        fmap (const (Just (1 :: Double))) xs === xs /: xs


testPropEqMaybe :: TestTree
testPropEqMaybe
    = testProperty "Broadcastable equality with holes (==:)" $ property $ do
        m1 <- forAll $ Gen.map (Range.linear 0 100) ((,) <$> Gen.text (Range.singleton 2) Gen.alpha <*> Gen.double (Range.linearFrac (-500) 500))
        let xs = fromStrictMap m1
        (xs ==: xs) === fromList (map (, Just True) (Index.toAscList (index xs)))


testPropNotEqMaybe :: TestTree
testPropNotEqMaybe
    = testProperty "Broadcastable non-equality with holes (/=:)" $ property $ do
        m1 <- forAll $ Gen.map (Range.linear 0 100) ((,) <$> Gen.text (Range.singleton 2) Gen.alpha <*> Gen.double (Range.linearFrac (-500) 500))
        let xs = fromStrictMap m1
        (xs /=: fmap (+1) xs) === fromList (map (, Just True) (Index.toAscList (index xs)))


testPropPlusMatched :: TestTree
testPropPlusMatched
    = testProperty "Broadcastable addition without holes (+|)" $ property $ do
        m1 <- forAll $ Gen.map (Range.linear 0 100) ((,) <$> Gen.text (Range.singleton 2) Gen.alpha <*> Gen.int (Range.linear 0 1000))
        c  <- forAll $ Gen.int (Range.linear 0 100)

        let xs = fromStrictMap m1
            expectation = fmap (+c) xs
        expectation === (xs +| c)
        expectation === (c +| xs)
        fmap (*2) xs === xs +| xs


testPropMinusMatched :: TestTree
testPropMinusMatched
    = testProperty "Broadcastable subtraction without holes (-|)" $ property $ do
        m1 <- forAll $ Gen.map (Range.linear 0 100) ((,) <$> Gen.text (Range.singleton 2) Gen.alpha <*> Gen.int (Range.linear 0 1000))
        c  <- forAll $ Gen.int (Range.linear 0 100)

        let xs = fromStrictMap m1
        fmap (\x -> x - c) xs === (xs -| c)
        fmap (c -) xs         === (c -| xs)
        fmap (const (0 :: Int)) xs === xs -| xs


testPropMultMatched :: TestTree
testPropMultMatched
    = testProperty "Broadcastable multiplication without holes (*|)" $ property $ do
        m1 <- forAll $ Gen.map (Range.linear 0 100) ((,) <$> Gen.text (Range.singleton 2) Gen.alpha <*> Gen.int (Range.linear 0 1000))
        c  <- forAll $ Gen.int (Range.linear 0 100)

        let xs = fromStrictMap m1
        fmap (*c) xs === (xs *| c)
        fmap (*c) xs === (c *| xs)


testPropDivMatched :: TestTree
testPropDivMatched
    = testProperty "Broadcastable division without holes (/|)" $ property $ do
        m1 <- forAll $ Gen.map (Range.linear 0 100) ((,) <$> Gen.text (Range.singleton 2) Gen.alpha <*> Gen.double (Range.linearFrac (-500) 500))
        c  <- forAll $ Gen.double (Range.linearFrac (-500) 500)

        let xs = fromStrictMap m1
        forM_ (index xs) $ \k -> do
            (fmap (/c) xs `at` k) `approx` ( (xs /| c) `at` k)
            (fmap (c/) xs `at` k) `approx` ( (c /| xs) `at` k)

        fmap (const (Just (1 :: Double))) xs === xs /: xs


testPropEqMatched :: TestTree
testPropEqMatched
    = testProperty "Broadcastable equality without holes (==|)" $ property $ do
        m1 <- forAll $ Gen.map (Range.linear 0 100) ((,) <$> Gen.text (Range.singleton 2) Gen.alpha <*> Gen.double (Range.linearFrac (-500) 500))
        let xs = fromStrictMap m1
        (xs ==| xs) === fromList (map (, True) (Index.toAscList (index xs)))


testPropNotEqMatched :: TestTree
testPropNotEqMatched
    = testProperty "Broadcastable non-equality without holes (/=|)" $ property $ do
        m1 <- forAll $ Gen.map (Range.linear 0 100) ((,) <$> Gen.text (Range.singleton 2) Gen.alpha <*> Gen.double (Range.linearFrac (-500) 500))
        let xs = fromStrictMap m1
        (xs /=| fmap (+1) xs) === fromList (map (, True) (Index.toAscList (index xs)))