module Main (main) where

import qualified Test.Data.Series
import qualified Test.Data.Series.Aggregation
import qualified Test.Data.Series.Broadcast
import qualified Test.Data.Series.Conversion
import qualified Test.Data.Series.Definition
import qualified Test.Data.Series.IO
import qualified Test.Data.Series.Numeric
import qualified Test.Data.Series.View

import           Test.Tasty ( defaultMain, testGroup )

main :: IO ()
main = defaultMain $ testGroup "Test suite" [ Test.Data.Series.tests
                                            , Test.Data.Series.Aggregation.tests
                                            , Test.Data.Series.Broadcast.tests
                                            , Test.Data.Series.Conversion.tests 
                                            , Test.Data.Series.Definition.tests
                                            , Test.Data.Series.IO.tests
                                            , Test.Data.Series.Numeric.tests
                                            , Test.Data.Series.View.tests
                                            ]
