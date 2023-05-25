module Main (main) where

import qualified Test.Data.Series.Finance.Metrics

import           Test.Tasty ( defaultMain, testGroup )

main :: IO ()
main = defaultMain $ testGroup "Test suite" [ Test.Data.Series.Finance.Metrics.tests
                                            ]
