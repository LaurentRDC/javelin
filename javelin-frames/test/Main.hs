module Main (main) where

import qualified Test.Data.Frame

import           Test.Tasty ( defaultMain, testGroup )

main :: IO ()
main = defaultMain 
     $ testGroup "Test suite" 
                 [ Test.Data.Frame.tests
                 ]