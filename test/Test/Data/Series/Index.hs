
module Test.Data.Series.Index (tests) where


import qualified Data.Series.Index    as Index
import qualified Data.Set             as Set

import           Hedgehog             ( property, forAll, tripping, assert )
import qualified Hedgehog.Gen         as Gen
import qualified Hedgehog.Range       as Range


import           Test.Tasty           ( testGroup, TestTree ) 
import           Test.Tasty.Hedgehog  ( testProperty )


tests :: TestTree
tests = testGroup "Data.Series.Index" [ testPropFromToSet
                                      , testPropFromToList
                                      , testPropFromToAscList
                                      , testPropMemberNotMember
                                      ]


testPropFromToSet :: TestTree
testPropFromToSet = testProperty "fromSet / toSet" $ property $ do
    ms <- forAll $ Gen.list (Range.linear 0 50) ((,) <$> Gen.alpha <*> Gen.alpha)
    tripping (Set.fromList ms) Index.fromSet (Just . Index.toSet)


testPropFromToList :: TestTree
testPropFromToList = testProperty "fromList / toList" $ property $ do
    ms <- forAll $ Gen.list (Range.linear 0 50) ((,) <$> Gen.alpha <*> Gen.alpha)
    let noDups = Set.toList $ Set.fromList ms
    tripping noDups Index.fromList (Just . Index.toList)


testPropFromToAscList :: TestTree
testPropFromToAscList = testProperty "fromAscList / toAscList" $ property $ do
    ms <- forAll $ Gen.list (Range.linear 0 50) ((,) <$> Gen.alpha <*> Gen.alpha)
    let noDups = Set.toAscList $ Set.fromList ms
    tripping noDups Index.fromAscList (Just . Index.toAscList)


testPropMemberNotMember :: TestTree
testPropMemberNotMember = testProperty "elements are either a member or not a member of the index" $ property $ do
    ms <- forAll $ Gen.list (Range.linear 0 50) (Gen.int (Range.linear (-100) 100))
    k  <- forAll $ Gen.int (Range.linear (-100) 100)

    let ix = Index.fromList ms
    assert $ (k `Index.member` ix) /= (k `Index.notMember` ix) 