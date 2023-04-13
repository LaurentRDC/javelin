module Main (main) where

import qualified Test.Data.Series.IO

import           Test.Tasty ( defaultMain, testGroup )

main :: IO ()
main = defaultMain $ testGroup "Test suite" [ Test.Data.Series.IO.tests
                                            ]
