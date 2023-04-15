module Main (main) where

import qualified Test.Data.Series
import qualified Test.Data.Series.Generic.Aggregation
import qualified Test.Data.Series.Generic.Definition
import qualified Test.Data.Series.Index
import qualified Test.Data.Series.Generic.Numeric
import qualified Test.Data.Series.Generic.View
import qualified Test.Data.Series.Generic.Windowing
import qualified Test.Data.Series.Generic.Zip

import           Test.Tasty ( defaultMain, testGroup )

main :: IO ()
main = defaultMain $ testGroup "Test suite" [ Test.Data.Series.tests
                                            , Test.Data.Series.Index.tests
                                            , Test.Data.Series.Generic.Aggregation.tests
                                            , Test.Data.Series.Generic.Definition.tests
                                            , Test.Data.Series.Generic.Numeric.tests
                                            , Test.Data.Series.Generic.View.tests
                                            , Test.Data.Series.Generic.Windowing.tests
                                            , Test.Data.Series.Generic.Zip.tests
                                            ]
