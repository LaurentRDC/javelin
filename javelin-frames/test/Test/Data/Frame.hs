{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
module Test.Data.Frame (tests) where

import           Control.Monad (guard, forM_)

import           Data.Frame as Frame hiding (length)
import           Data.Function (on)
import qualified Data.List as List (intersperse)
import qualified Data.Set as Set
import qualified Data.Vector as Vector

import           GHC.Generics (Generic)

import           Hedgehog             ( property, forAll, (===), assert )
import qualified Hedgehog.Gen         as Gen
import qualified Hedgehog.Range       as Range

import           Test.Tasty           ( testGroup, TestTree ) 
import           Test.Tasty.Hedgehog  ( testProperty )
import           Test.Tasty.HUnit     ( testCase, assertEqual )     

tests :: TestTree
tests = testGroup "Data.Frame" [ testToFromRowsTripping
                               , testLookup
                               , testFields
                               , testSortRowsBy
                               , testSortRowsByKey
                               , testMergeWithStrategy
                               , testDisplay
                               ]

data User f
    -- Note that the fields are NOT ordered alphabetically,
    -- which is important for the display test cases.
    -- We want to present dataframes as the user intended it.
    = MkUser { userName :: Column f String
             , userAge  :: Column f Int
             }
    deriving (Generic)


instance Frameable User
deriving instance Show (Row User)
instance Show (Frame User) where show = Frame.display
deriving instance Eq (Row User)
deriving instance Eq (Frame User)

instance Indexable User where
    type Key User = String
    index = userName

testToFromRowsTripping :: TestTree
testToFromRowsTripping = testProperty "Ensure that `toRows` and `fromRows` are inverses" $ property $ do
    users <- forAll $ Vector.fromList <$> 
                        Gen.list (Range.linear 0 100) 
                            (MkUser <$> Gen.string (Range.linear 0 100) Gen.alpha
                            <*> Gen.integral (Range.linear 10 25)
                            )
    users === toRows (fromRows users)

testLookup :: TestTree
testLookup = testProperty "Ensure that `lookup` works" $ property $ do
    users <- forAll $ Vector.fromList <$> 
                        Gen.list (Range.linear 0 100) 
                            (MkUser <$> Gen.string (Range.linear 0 100) Gen.alpha
                            <*> Gen.integral (Range.linear 10 25)
                            )
    
    -- This property only makes sense for a unique index
    guard (unique (Vector.map userName users))

    let df = fromRows users

    forM_ users $ \user -> do
        Frame.lookup (userName user) df === Just user
    
    where
        unique :: Ord a => Vector.Vector a -> Bool
        unique vs = length (Set.fromList (Vector.toList vs)) == Vector.length vs


testFields :: TestTree
testFields = testCase "Appropriately accessing field names and values" $ do
    let row = MkUser "Alice" 37
    assertEqual mempty ([("userName", "\"Alice\""), ("userAge", "37")]) (fields row)


testSortRowsBy :: TestTree
testSortRowsBy 
    = testGroup "sortRowsBy" 
        [ testSortRowsByUnit
        , testSortRowsByIdempotence
        ]
    
    where
        testSortRowsByUnit :: TestTree
        testSortRowsByUnit = testCase "sorting rows" $ do
            let frame = fromRows [ MkUser "Clara" 39
                                 , MkUser "Bob" 38
                                 , MkUser "David" 40
                                 , MkUser "Alice" 37
                                 ]
                expectation = fromRows [ MkUser "Alice" 37
                                       , MkUser "Bob" 38
                                       , MkUser "Clara" 39
                                       , MkUser "David" 40
                                       ]
            
            assertEqual mempty expectation (sortRowsBy (compare `on` userName) frame)
        
        testSortRowsByIdempotence :: TestTree
        testSortRowsByIdempotence = testProperty "Sorting rows is idempotent" $ property $ do
            users <- forAll $ Vector.fromList <$> 
                                Gen.list (Range.linear 0 100) 
                                    (MkUser <$> Gen.string (Range.linear 0 100) Gen.alpha
                                    <*> Gen.integral (Range.linear 10 25)
                                    )
            
            -- This property only makes sense for a unique index
            guard (unique (Vector.map userName users))

            let df = fromRows users
                sorted = sortRowsBy (compare `on` userName) df
            
            sorted === (sortRowsBy (compare `on` userName) sorted)

            where
                unique :: Ord a => Vector.Vector a -> Bool
                unique vs = length (Set.fromList (Vector.toList vs)) == Vector.length vs


testSortRowsByKey :: TestTree
testSortRowsByKey 
    = testGroup "sortRowsByKey" 
        [ testSortRowsByKeyUnit
        , testSortRowsByKeyUniqueOnUnit
        , testSortRowsByKeyIdempotence
        , testSortRowsByKeyUniqueOnIdempotence
        ]
    
    where
        testSortRowsByKeyUnit :: TestTree
        testSortRowsByKeyUnit = testCase "sorting rows" $ do
            let frame = fromRows [ MkUser "Clara" 39
                                 , MkUser "Bob" 38
                                 , MkUser "David" 40
                                 , MkUser "Alice" 37
                                 ]
                expectation = fromRows [ MkUser "Alice" 37
                                       , MkUser "Bob" 38
                                       , MkUser "Clara" 39
                                       , MkUser "David" 40
                                       ]
            
            assertEqual mempty expectation (sortRowsByKey frame)

        testSortRowsByKeyUniqueOnUnit :: TestTree
        testSortRowsByKeyUniqueOnUnit = testCase "sorting rows by mapping keys" $ do
            let frame = fromRows [ MkUser "Clarice" 39
                                 , MkUser "Bobby" 38
                                 , MkUser "Davidson" 40
                                 , MkUser "Abe" 37
                                 ]
                expectation = fromRows [ MkUser "Abe" 37
                                       , MkUser "Bobby" 38
                                       , MkUser "Clarice" 39
                                       , MkUser "Davidson" 40
                                       ]
            
            assertEqual mempty expectation (sortRowsByKeyUniqueOn (length) frame)
        
        testSortRowsByKeyIdempotence :: TestTree
        testSortRowsByKeyIdempotence = testProperty "Sorting rows by key is idempotent" $ property $ do
            users <- forAll $ Vector.fromList <$> 
                                Gen.list (Range.linear 0 100) 
                                    (MkUser <$> Gen.string (Range.linear 0 100) Gen.alpha
                                    <*> Gen.integral (Range.linear 10 25)
                                    )

            let df = fromRows users
                sorted = sortRowsByKey df
            
            sorted === (sortRowsByKey sorted)


        testSortRowsByKeyUniqueOnIdempotence :: TestTree
        testSortRowsByKeyUniqueOnIdempotence 
            = testProperty "Sorting rows by mapping key is idempotent" 
                $ property 
                    $ do
            users <- forAll $ Vector.fromList <$> 
                                Gen.list (Range.linear 0 100) 
                                    (MkUser <$> Gen.string (Range.linear 0 100) Gen.alpha
                                    <*> Gen.integral (Range.linear 10 25)
                                    )

            let df = fromRows users
                sorted = sortRowsByKeyUniqueOn length df
            
            sorted === (sortRowsByKeyUniqueOn length sorted)


testMergeWithStrategy :: TestTree
testMergeWithStrategy 
    = testGroup "mergeWithStrategy"
    [ testMergeWithStrategyUnion
    , testMergeWithStrategySelf
    , testMergeWithStrategyOn
    ]
    where
        testMergeWithStrategyUnion :: TestTree
        testMergeWithStrategyUnion 
            = testProperty "The index of a merged dataframe contains a subset of the union of the indices" 
            $ property 
            $ do

                users1 <- fmap fromRows <$> forAll $ 
                                    Gen.list (Range.linear 0 50)
                                        (MkUser <$> Gen.string (Range.linear 0 100) Gen.alpha
                                        <*> Gen.integral (Range.linear 10 25)
                                        )

                users2 <- fmap fromRows <$> forAll $ 
                                    Gen.list (Range.linear 0 25)
                                        (MkUser <$> Gen.string (Range.linear 0 100) Gen.alpha
                                        <*> Gen.integral (Range.linear 10 25)
                                        )

                let merged = Frame.mergeWithStrategy strategy users1 users2
                    mergedIx = Set.fromList $ Vector.toList (index merged) 
                    ix1 = Set.fromList $ Vector.toList (index users1)
                    ix2 = Set.fromList $ Vector.toList (index users2)

                
                assert (mergedIx `Set.isSubsetOf` (ix1 `Set.union` ix2))
        
            where
                strategy :: String -> These (Row User) (Row User) -> Maybe (Row User)
                strategy _ (This left) = Just left
                strategy _ (That right) = Just right
                strategy name (These _ _) = Just $ MkUser name 18
        
        testMergeWithStrategySelf :: TestTree
        testMergeWithStrategySelf 
            = testProperty "Merging a dataframe onto itself should be the identity function if the index is unique"
            $ property $ do
                users <- fmap fromRows <$> forAll $ 
                                    Gen.list (Range.linear 0 50)
                                        (MkUser <$> Gen.string (Range.linear 0 100) Gen.alpha
                                        <*> Gen.integral (Range.linear 10 25)
                                        )

                Frame.mergeWithStrategy (Frame.matchedStrategy (\_ u _ -> u)) users users === Frame.sortRowsByKeyUnique users
                
        testMergeWithStrategyOn :: TestTree
        testMergeWithStrategyOn = testCase "mergeWithStrategyOn" $ do
            let users1 = fromRows [ MkUser "A" 39
                                  , MkUser "BB" 98
                                  , MkUser "CCC" 51
                                  , MkUser "DDDD" 37
                                  ]
                users2 = fromRows [ MkUser "X" 1
                                  , MkUser "XXX" 3
                                  , MkUser "XX" 2
                                  , MkUser "XXXXXXXXX" 37
                                  ]

                expectation = fromRows [ MkUser "1" (39 + 1)
                                       , MkUser "2" (98 + 2)
                                       , MkUser "3" (51 + 3)
                                       ]
            -- We join the frames on the LENGTH of the names.
            assertEqual mempty expectation 
                $ Frame.mergeWithStrategyOn length
                                            length
                                            (Frame.matchedStrategy $ \k (MkUser _ age1) (MkUser _ age2) -> MkUser (show k) (age1 + age2))
                                            users1
                                            users2 

testDisplay :: TestTree
testDisplay = 
    let frame = fromRows [ MkUser "Alice" 37
                         , MkUser "Bob" 38
                         , MkUser "Clara" 39
                         , MkUser "David" 40
                         ]
    in testGroup "displaytWith" [
        testCase "Appropriately displaying all rows" $ do
                let displayed = Frame.displayWith (Frame.defaultDisplayOptions {maximumNumberOfRows = 4}) frame
                    expectation = unlines' [ "userName | userAge"
                                           , "-------- | -------"
                                           , " \"Alice\" |      37"
                                           , "   \"Bob\" |      38"
                                           , " \"Clara\" |      39"
                                           , " \"David\" |      40"
                                           ]
            
                assertEqual mempty expectation displayed,
        testCase "Appropriately eliding some rows" $ do
                let displayed = Frame.displayWith (Frame.defaultDisplayOptions {maximumNumberOfRows = 2}) frame
                    expectation = unlines' [ "userName | userAge"
                                           , "-------- | -------"
                                           , " \"Alice\" |      37"
                                           , "     ... |     ..."
                                           , " \"David\" |      40"
                                           ]
            
                assertEqual mempty expectation displayed
    ]
    where
        unlines' = mconcat . List.intersperse "\n"