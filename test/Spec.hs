module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Dial

main :: IO ()
main = putStrLn $ "Test suite: "  ++ Dial.stripDashes "1-800-COMCAST"
