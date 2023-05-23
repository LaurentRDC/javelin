
module Test.Utils ( approx, assertApproxEqual ) where
import           Control.Monad        ( unless )
import           Data.AEq             ( AEq((~==)))
import           Hedgehog             ( MonadTest, diff )
import           Hedgehog.Internal.Source ( HasCallStack, withFrozenCallStack ) 
import           Test.HUnit           ( assertFailure, Assertion )


-- | Fails the test if the two arguments provided are not equal to within `epsilon`.
approx :: (MonadTest m, AEq a, Show a, HasCallStack) => a -> a -> m ()
approx x y =
  withFrozenCallStack $
    diff x (~==) y

-- | Asserts that the specified actual value is approximately equal to the
-- expected value. The output message will contain the prefix, the expected
-- value, the actual value, and the maximum margin of error.
--
-- If the prefix is the empty string (i.e., @\"\"@), then the prefix is omitted
-- and only the expected and actual values are output.
--
-- This function was borrowed from the hunit-approx package, version 1.1.1.1
assertApproxEqual :: (HasCallStack, Ord a, Num a, Show a)
                  => String -- ^ The message prefix
                  -> a      -- ^ Maximum allowable margin of error
                  -> a      -- ^ The expected value
                  -> a      -- ^ The actual value
                  -> Assertion
assertApproxEqual preface epsilon expected actual =
  unless (abs (actual - expected) <= epsilon) (assertFailure msg)
  where msg = (if null preface then "" else preface ++ "\n") ++
              "expected: " ++ show expected ++ "\n but got: " ++ show actual ++
              "\n (maximum margin of error: " ++ show epsilon ++ ")"