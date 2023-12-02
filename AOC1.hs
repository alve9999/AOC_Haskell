import System.IO
import Control.Exception (handle)
import GHC.Data.ShortText (ShortText(contents))
import Data.Char (isDigit)
import Text.Read (Lexeme(Char))
import Debug.Trace
import qualified Data.Map as Map
import Distribution.Compat.CharParsing (CharParsing(string))

filterStringDigit :: String -> String
filterStringDigit word = case word of
  "zero" -> "0"
  "one" -> "1"
  "two" -> "2"
  "three" -> "3"
  "four" -> "4"
  "five" -> "5"
  "six" -> "6"
  "seven" -> "7"
  "eight" -> "8"
  "nine" -> "9"
  _ -> ""

replaceDigit :: String -> String -> String
replaceDigit [] _ = ""
replaceDigit (x:xs) str = let a = filterStringDigit (str++[x]) in case a of
    "" -> replaceDigit xs (str++[x])
    _ -> a

insertDigits :: String -> String -> String
insertDigits str [] = str
insertDigits out ins = insertDigits (out++[head ins] ++ replaceDigit ins "") (tail ins)

filterInts :: [Char] -> [Char]
filterInts = filter isDigit

kFAL :: [a] -> [a]
kFAL [] = []
kFAL [x] = [x,x]
kFAL (x:xs) = [x, last xs]



main :: IO ()
main = do
    handle <- openFile "AOC1.txt" ReadMode
    contents <- hGetContents handle

    let line = lines contents

    let intermitent = map (insertDigits "") line 

    let ints = map filterInts intermitent

    let res = map kFAL ints

    let result = sum (map read res::[Int])
    putStrLn $ unlines $ zipWith3 (\a b c -> a ++ " " ++ b ++ " " ++ c ++ " ") (line) (intermitent) (ints) 
    print result

    hClose handle