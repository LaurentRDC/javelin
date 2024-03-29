
module Test.Data.Series.Index (tests) where

import qualified Data.Series.Index    as Index
import qualified Data.Series.Index.Internal as Index.Internal
import qualified Data.Set             as Set
import qualified Data.Vector          as Vector

import           Hedgehog             ( property, forAll, tripping, assert, (===) )
import qualified Hedgehog.Gen         as Gen
import qualified Hedgehog.Range       as Range


import           Test.Tasty           ( testGroup, TestTree ) 
import           Test.Tasty.Hedgehog  ( testProperty )


tests :: TestTree
tests = testGroup "Data.Series.Index" [ testPropRange
                                      , testPropFromToSet
                                      , testPropFromToList
                                      , testPropFromToAscList
                                      , testPropFromToVector
                                      , testPropFromToAscVector
                                      , testPropMemberNotMember
                                      , testPropIndexed
                                      , testPropFilter
                                      ]


testPropRange :: TestTree
testPropRange = testProperty "range always includes the start, and all elements less than/equal to end" $ property $ do
    start <- forAll $ Gen.int (Range.linear 0 50)
    end   <- forAll $ Gen.int (Range.linear 51 100)
    step  <- forAll $ Gen.int (Range.linear 1 5)

    let ix = Index.range (+step) start end 

    assert $ start `Index.member` ix
    assert $ maximum ix <= end

    if (end - start) `mod` step == 0
        then assert (end `Index.member` ix)
        else assert (end `Index.notMember` ix)


testPropFromToSet :: TestTree
testPropFromToSet = testGroup "conversion to/from Set" 
    [ testProperty "fromSet / toSet" $ property $ do
        ms <- forAll $ Gen.list (Range.linear 0 50) ((,) <$> Gen.alpha <*> Gen.alpha)
        tripping (Set.fromList ms) Index.fromSet (Just . Index.toSet)
    , testProperty "toIndex / fromIndex" $ property $ do
        ms <- forAll $ Gen.list (Range.linear 0 50) ((,) <$> Gen.alpha <*> Gen.alpha)
        tripping (Set.fromList ms) (Index.toIndex :: Set.Set (Char, Char) -> Index.Index (Char, Char)) (Just . Index.fromIndex)
    ]


testPropFromToList :: TestTree
testPropFromToList = testGroup "conversion to/from list" 
    [ testProperty "fromList / toAscList" $ property $ do
        ms <- forAll $ Gen.list (Range.linear 0 50) ((,) <$> Gen.alpha <*> Gen.alpha)
        let index = Index.fromList ms
        tripping index (reverse . Index.toAscList) (Just . Index.fromList)
    , testProperty "toIndex / fromIndex" $ property $ do
        ms <- forAll $ Gen.list (Range.linear 0 50) ((,) <$> Gen.alpha <*> Gen.alpha)
        let index = Index.toIndex ms :: Index.Index (Char, Char)
        tripping index (reverse . Index.fromIndex) (Just . (Index.toIndex :: [(Char, Char)] -> Index.Index (Char, Char)))
    ]


testPropFromToAscList :: TestTree
testPropFromToAscList = testProperty "fromAscList / toAscList" $ property $ do
    ms <- forAll $ Gen.list (Range.linear 0 50) ((,) <$> Gen.alpha <*> Gen.alpha)
    let index = Index.fromList ms
    tripping index Index.toAscList (Just . Index.Internal.fromAscList)


testPropFromToVector :: TestTree
testPropFromToVector = testGroup "conversion to/from Vector"
    [ testProperty "fromVector / toAscVector" $ property $ do
        ms <- forAll $ Gen.list (Range.linear 0 50) ((,) <$> Gen.alpha <*> Gen.alpha)
        let index = Index.fromList ms
        tripping index (Vector.reverse . Index.toAscVector) (Just . Index.fromVector)
    , testProperty "toIndex / fromIndex" $ property $ do
        ms <- forAll $ Gen.list (Range.linear 0 50) ((,) <$> Gen.alpha <*> Gen.alpha)
        let index = Index.toIndex ms :: Index.Index (Char, Char)
        tripping index (Vector.reverse . Index.fromIndex) (Just . (Index.toIndex :: Vector.Vector (Char, Char) -> Index.Index (Char, Char)))
    ]


testPropFromToAscVector :: TestTree
testPropFromToAscVector = testProperty "fromAscVector / toAscVector" $ property $ do
    ms <- forAll $ Gen.list (Range.linear 0 50) ((,) <$> Gen.alpha <*> Gen.alpha)
    let index = Index.fromList ms
    tripping index (Index.toAscVector :: Index.Index (Char, Char) -> Vector.Vector (Char, Char)) (Just . Index.Internal.fromAscVector)


testPropMemberNotMember :: TestTree
testPropMemberNotMember = testProperty "elements are either a member or not a member of the index" $ property $ do
    ms <- forAll $ Gen.list (Range.linear 0 50) (Gen.int (Range.linear (-100) 100))
    k  <- forAll $ Gen.int (Range.linear (-100) 100)

    let ix = Index.fromList ms
    assert $ (k `Index.member` ix) /= (k `Index.notMember` ix)


testPropIndexed :: TestTree
testPropIndexed = testProperty "indexed works just like for Vectors" $ property $ do
    ms <- forAll $ Gen.list (Range.linear 0 50) (Gen.int (Range.linear (-100) 100))

    let ix = Index.fromList ms
    
    Index.toAscVector (Index.indexed ix) === Vector.indexed (Index.toAscVector ix)


testPropFilter :: TestTree
testPropFilter = testProperty "filter works just like for Sets" $ property $ do
    ms <- forAll $ Gen.list (Range.linear 0 50) (Gen.int (Range.linear (-100) 100))

    let ss = Set.fromList ms
        ix = Index.fromSet ss
    
    Index.fromSet (Set.filter even ss) === Index.filter even ix
