module TextProc where

import qualified Data.List as L
import qualified Data.Char as C

count :: Eq a => a -> [a] -> Int
count x [] = 0
count x (y:ys) | x == y = succ (count x ys)
               | otherwise = count x ys

average :: [Int] -> Float
average [] = error "Empty List"
average xs = fromRational (toRational (sum xs) / toRational (length xs))

averageLength :: String -> Float
averageLength str = average $ map length $ words str

prefix :: Eq a => [a] -> [a] -> Bool
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (x==y) && prefix xs ys

sublist :: Eq a => [a] -> [a] -> Bool
sublist [] ys = True
sublist xs [] = False
sublist xs ys = (prefix xs ys) || sublist xs (tail ys)

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (\y -> (y /= x)) xs)

-- to remove everything but alphanumeric characters
preprocess :: String -> String
preprocess = (map C.toLower) . filter (`notElem` "?;:.,")


-- to text process given sonnets
process :: String -> [String]
process = L.sort . nub . words

cnt :: String -> [(String, Int)]
cnt sonnet = [ (x,y) | x <- (process . preprocess) sonnet,
               y <- [count x ((words . preprocess) sonnet)],
               y > 1]
