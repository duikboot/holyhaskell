module Main where

import Test.Tasty (defaultMain,testGroup,TestTree)

import Holyhaskell.Swallow.Test
import Holyhaskell.Coconut.Test

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests"
            [ swallowSuite
            , coconutSuite
            ]
