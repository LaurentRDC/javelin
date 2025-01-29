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

import           Hedgehog             ( property, forAll, (===) )
import qualified Hedgehog.Gen         as Gen
import qualified Hedgehog.Range       as Range

import           Test.Tasty           ( testGroup, TestTree ) 
import           Test.Tasty.Hedgehog  ( testProperty )
import           Test.Tasty.HUnit     

tests :: TestTree
tests = testGroup "Data.Frame" [ testToFromRowsTripping
                               , testLookup
                               , testFields
                               , testSortRowsBy
                               , testSortRowsByKey
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
        , testSortRowsByKeyIdempotence
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
        
        testSortRowsByKeyIdempotence :: TestTree
        testSortRowsByKeyIdempotence = testProperty "Sorting rows by key is idempotent" $ property $ do
            users <- forAll $ Vector.fromList <$> 
                                Gen.list (Range.linear 0 100) 
                                    (MkUser <$> Gen.string (Range.linear 0 100) Gen.alpha
                                    <*> Gen.integral (Range.linear 10 25)
                                    )
            
            -- This property only makes sense for a unique index
            guard (unique (Vector.map userName users))

            let df = fromRows users
                sorted = sortRowsByKey df
            
            sorted === (sortRowsByKey sorted)

            where
                unique :: Ord a => Vector.Vector a -> Bool
                unique vs = length (Set.fromList (Vector.toList vs)) == Vector.length vs


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