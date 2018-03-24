module Dial
    ( enter,
    removeDash
    ) where

import Data.Char (ord, toLower, isNumber, intToDigit, isPunctuation)
import System.IO
import Data.List


enter = do
    putStrLn "Enter a phone number"
    num <- getLine
    putStrLn $ "You may dial " ++ num

stripDashes :: String -> String
stripDashes xs = [ x | x <- xs, not (elem x  "-") ]

insertDashes :: String -> String
insertDashes xs = take 1 ls ++ ['-'] ++ take 3 (drop 1 ls) ++ ['-'] ++ take 3 (drop 4 ls) ++ ['-'] ++ take 4 (drop 7 ls) 

getDigit :: Char -> Int
getDigit letter
    | (ord letter >= 97) && (ord letter <= 99) = 2
    | ord letter <= 102 = 3
    | ord letter <= 105 = 4
    | ord letter <= 108 = 5
    | ord letter <= 111 = 6
    | ord letter <= 115 = 7
    | ord letter <= 118 = 8
    | ord letter <= 122 = 9
    | otherwise = 0

convertLetters :: String -> String
convertLetters xs = map (\x -> if (isNumber x) then x else (intToDigit $ getDigit $ toLower x)) xs
