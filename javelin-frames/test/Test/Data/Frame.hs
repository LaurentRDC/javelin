{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
module Test.Data.Frame (tests) where

import           Control.Monad (guard, forM_)
import           Data.Frame as Frame hiding (length)
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
deriving instance Eq (Row User)

testToFromRowsTripping :: TestTree
testToFromRowsTripping = testProperty "Ensure that `toRows` and `fromRows` are inverses" $ property $ do
    users <- forAll $ Vector.fromList <$> 
                        Gen.list (Range.linear 0 100) 
                            (MkUser <$> Gen.string (Range.linear 0 100) Gen.alpha
                            <*> Gen.integral (Range.linear 10 25)
                            )
    users === toRows (fromRows users)


instance Indexable User where
    type Key User = String
    index = userName


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