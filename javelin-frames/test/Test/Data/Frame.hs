{-# LANGUAGE DeriveGeneric #-}
module Test.Data.Frame (tests) where

import           Data.Frame
import qualified Data.Vector as Vector

import           GHC.Generics (Generic)

import           Hedgehog             ( property, forAll, (===) )
import qualified Hedgehog.Gen         as Gen
import qualified Hedgehog.Range       as Range

import           Test.Tasty           ( testGroup, TestTree ) 
import           Test.Tasty.Hedgehog  ( testProperty )

tests :: TestTree
tests = testGroup "Data.Frame" [ testToFromRowsTripping ]

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