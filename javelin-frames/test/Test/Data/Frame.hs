{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
module Test.Data.Frame (tests) where

import           Control.Monad (guard, forM_)
import           Data.Frame as Frame
import qualified Data.Set as Set
import qualified Data.Vector as Vector

import           GHC.Generics (Generic)

import           Hedgehog             ( property, forAll, (===) )
import qualified Hedgehog.Gen         as Gen
import qualified Hedgehog.Range       as Range

import           Test.Tasty           ( testGroup, TestTree ) 
import           Test.Tasty.Hedgehog  ( testProperty )

tests :: TestTree
tests = testGroup "Data.Frame" [ testToFromRowsTripping
                               , testLookup
                               ]

data User f
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