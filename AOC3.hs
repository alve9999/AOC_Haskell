import System.IO
import Data.List
import Data.Char (isDigit)
import Debug.Trace
import GHC.Data.ShortText (ShortText(contents))

findChars :: String -> String -> [Int] -> [Int]
findChars str [] int = int
findChars str (x:xs) int = findChars str xs (int ++ elemIndices x str)

getAdjIndices :: Int -> [Int]
getAdjIndices ind = let
    row = ind `div` 140
    col = ind `mod` 140 in
     [r * 140 + c | r <- [row-1,row,row+1],c <- [col-1,col,col+1]]


findInt :: Int -> String -> String -> (Int,Int)
findInt x content str
    | isDigit (str !! x) =
        findInt (x+1) content (str++[str !! x])
    | str=="" = (x,0)
    | otherwise = (x,read str)

addInts :: [Int] -> Int -> String -> Int -> Int
addInts [] searched content res = res
addInts (x:xs) searched content res
    | x<=searched = addInts xs searched content res
    | otherwise = let 
        (limit,add)=findInt x content "" in
            addInts xs limit content res+add




main = do
    handle <- openFile "AOC3.txt" ReadMode
    contents <- hGetContents handle

    let indecies = findChars contents "%/*@=+&-" []
    let search = map getAdjIndices indecies
    let joinedIndecies = concat search
    let unique = nub joinedIndecies
    let sortUnique = sort joinedIndecies
    print sortUnique
    let result = addInts sortUnique 0 contents 0
    print result

    hClose handle