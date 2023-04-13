
module Test.Utils ( approx ) where

import           Data.AEq             ( AEq((~==)))
import           Hedgehog             ( MonadTest, diff )
import           Hedgehog.Internal.Source ( HasCallStack, withFrozenCallStack ) 


-- | Fails the test if the two arguments provided are not equal to within `epsilon`.
approx :: (MonadTest m, AEq a, Show a, HasCallStack) => a -> a -> m ()
approx x y =
  withFrozenCallStack $
    diff x (~==) y