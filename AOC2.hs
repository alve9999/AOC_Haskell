{-# HLINT ignore "Redundant bracket" #-}
import System.IO
import Control.Exception (handle)
import GHC.Data.ShortText (ShortText(contents))
import Data.Char (isDigit)
import Text.Read (Lexeme(Char))
import Debug.Trace
import Data.List.Split


find :: String -> [[String]] -> Int
find str ((val:col):ys) = if (head col) == str
    then read val
    else find str ys
find str [] = 0

findMax :: String -> [[[[Char]]]] -> Int
findMax str input = maximum (map (find str) input)

main = do
    handle <- openFile "AOC2.txt" ReadMode
    contents <- hGetContents handle

    let games = map (tail . splitOn ":") (lines contents)
    let sets = map (map (splitOn ";")) games
    let ind = map (map (map (splitOn ","))) sets
    let i = map (map (map (map (splitOn " ")))) ind
    let j = map (map (map (map tail))) i
    let res = map (head) j

    let red = map (findMax "red") res
    let blue = map (findMax "blue") res
    let green = map (findMax "green") res

    let res2 = zipWith3 (\a b c -> a*b*c) red green blue
    print $ show $ sum res2

    hClose handle