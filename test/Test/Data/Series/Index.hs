
module Test.Data.Series.Index (tests) where


import qualified Data.Series.Index    as Index
import qualified Data.Set             as Set
import qualified Data.Vector          as Vector

import           Hedgehog             ( property, forAll, tripping, assert, (===) )
import qualified Hedgehog.Gen         as Gen
import qualified Hedgehog.Range       as Range


import           Test.Tasty           ( testGroup, TestTree ) 
import           Test.Tasty.Hedgehog  ( testProperty )


tests :: TestTree
tests = testGroup "Data.Series.Index" [ testPropFromToSet
                                      , testPropFromToList
                                      , testPropFromToAscList
                                      , testPropFromToVector
                                      , testPropFromToAscVector
                                      , testPropMemberNotMember
                                      , testPropFilter
                                      ]


testPropFromToSet :: TestTree
testPropFromToSet = testProperty "fromSet / toSet" $ property $ do
    ms <- forAll $ Gen.list (Range.linear 0 50) ((,) <$> Gen.alpha <*> Gen.alpha)
    tripping (Set.fromList ms) Index.fromSet (Just . Index.toSet)


testPropFromToList :: TestTree
testPropFromToList = testProperty "fromList / toAscList" $ property $ do
    ms <- forAll $ Gen.list (Range.linear 0 50) ((,) <$> Gen.alpha <*> Gen.alpha)
    let index = Index.fromList ms
    tripping index (reverse . Index.toAscList) (Just . Index.fromList)


testPropFromToAscList :: TestTree
testPropFromToAscList = testProperty "fromAscList / toAscList" $ property $ do
    ms <- forAll $ Gen.list (Range.linear 0 50) ((,) <$> Gen.alpha <*> Gen.alpha)
    let index = Index.fromList ms
    tripping index Index.toAscList (Just . Index.fromAscList)


testPropFromToVector :: TestTree
testPropFromToVector = testProperty "fromVector / toAscVector" $ property $ do
    ms <- forAll $ Gen.list (Range.linear 0 50) ((,) <$> Gen.alpha <*> Gen.alpha)
    let index = Index.fromList ms
    tripping index (Vector.reverse . Index.toAscVector) (Just . Index.fromVector)


testPropFromToAscVector :: TestTree
testPropFromToAscVector = testProperty "fromAscVector / toAscVector" $ property $ do
    ms <- forAll $ Gen.list (Range.linear 0 50) ((,) <$> Gen.alpha <*> Gen.alpha)
    let index = Index.fromList ms
    tripping index Index.toAscVector (Just . Index.fromAscVector)


testPropMemberNotMember :: TestTree
testPropMemberNotMember = testProperty "elements are either a member or not a member of the index" $ property $ do
    ms <- forAll $ Gen.list (Range.linear 0 50) (Gen.int (Range.linear (-100) 100))
    k  <- forAll $ Gen.int (Range.linear (-100) 100)

    let ix = Index.fromList ms
    assert $ (k `Index.member` ix) /= (k `Index.notMember` ix)


testPropFilter :: TestTree
testPropFilter = testProperty "filter works just like for Sets" $ property $ do
    ms <- forAll $ Gen.list (Range.linear 0 50) (Gen.int (Range.linear (-100) 100))

    let ss = Set.fromList ms
        ix = Index.fromSet ss
    
    (Index.fromSet (Set.filter even ss)) === (Index.filter even ix)
