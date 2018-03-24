module Dial
    ( enter,
    stripDashes
    ) where

import Data.Char (ord, toLower, isNumber, intToDigit, isPunctuation)
import Data.List


enter = do
    putStrLn "Enter a phone number"
    num <- getLine
    let converted = insertDashes $ convertLetters $ stripDashes num
    putStrLn $ "You may dial " ++ converted

stripDashes :: String -> String
stripDashes = filter (/= '-')

splitAtNs :: [Int] -> [a] -> [[a]]
splitAtNs []     xs = [xs]
splitAtNs (n:ns) xs = as : splitAtNs ns bs
    where (as, bs) = splitAt n xs

insertDashes :: String -> String
insertDashes = intercalate "-" . take 4 . splitAtNs [1,3,3,4]

getDigit :: Char -> Int
getDigit letter
    | letter >= 'a' && letter <= 'c' = 2
    | letter <= 'f' = 3
    | letter <= 'i' = 4
    | letter <= 'l' = 5
    | letter <= 'o' = 6
    | letter <= 's' = 7
    | letter <= 'v' = 8
    | letter <= 'z' = 9
    | otherwise = 0

convertLetters :: String -> String
convertLetters = map toDigit
  where
    toDigit x 
     | isNumber x = x 
     | otherwise  = intToDigit $ getDigit $ toLower x
