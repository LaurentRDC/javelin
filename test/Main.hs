module Main (main) where

import qualified Test.Data.Series 
import qualified Test.Data.Series.Conversion
import qualified Test.Data.Series.View

import           Test.Tasty ( defaultMain, testGroup )

main :: IO ()
main = defaultMain $ testGroup "Test suite" [ Test.Data.Series.tests
                                            , Test.Data.Series.Conversion.tests 
                                            , Test.Data.Series.View.tests
                                            ]
