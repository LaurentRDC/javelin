
module Test.Data.Series.Definition (tests) where

import           Data.Series          ( Series, fromList, fromStrictMap )

import           Hedgehog             ( property, forAll, (===) )
import qualified Hedgehog.Gen         as Gen
import qualified Hedgehog.Range       as Range

import           Test.Tasty           ( testGroup, TestTree ) 
import           Test.Tasty.Hedgehog  ( testProperty )
import           Test.Tasty.HUnit     ( testCase, assertEqual )

tests :: TestTree
tests = testGroup "Data.Series.Definition" [ testMappend
                                           , testPropMappendLikeMap
                                           ]


testMappend :: TestTree
testMappend = testCase "(<>)" $ do
    let (s1 :: Series Char Int) = fromList [('a', 1), ('b', 5)]
        (s2 :: Series Char Int) = fromList [('b', 10), ('x', 25)]
        expectation = fromList [('a', 1), ('b', 5),  ('x', 25)]
    
    assertEqual mempty expectation (s1 <> s2)


testPropMappendLikeMap :: TestTree
testPropMappendLikeMap 
    = testProperty "Mappend property similar to Data.Map.Strict" $ property $ do
        m1 <- forAll $ Gen.map (Range.linear 0 50) ((,) <$> Gen.int (Range.linear 0 1000)   <*> Gen.alpha)
        m2 <- forAll $ Gen.map (Range.linear 0 50) ((,) <$> Gen.int (Range.linear 500 1500) <*> Gen.alpha)

        fromStrictMap (m1 <> m2) === fromStrictMap m1 <> fromStrictMap m2