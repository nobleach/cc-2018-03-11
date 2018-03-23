module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Dial

main :: IO ()
main = putStrLn $ "Test suite: "  ++ Dial.removeDash "1-800-COMCAST"
