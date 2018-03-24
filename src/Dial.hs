module Dial
    ( enter,
    stripDashes
    ) where

import Data.Char (ord, toLower, isNumber, intToDigit, isPunctuation)
import System.IO
import Data.List


enter = do
    putStrLn "Enter a phone number"
    num <- getLine
    let converted = insertDashes $ convertLetters $ stripDashes num
    putStrLn $ "You may dial " ++ converted

stripDashes :: String -> String
stripDashes xs = [ x | x <- xs, not (elem x  "-") ]

insertDashes :: String -> String
insertDashes xs = take 1 xs ++ ['-'] ++ take 3 (drop 1 xs) ++ ['-'] ++ take 3 (drop 4 xs) ++ ['-'] ++ take 4 (drop 7 xs) 

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
