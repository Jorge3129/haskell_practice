{-# OPTIONS_GHC -Wall #-}
module P02_ListFunctions where

-- Задача 1 -----------------------------------------
sumFl :: [Integer] -> Integer
sumFl xs = foldl (+) 0 xs
  
-- Задача 2 ----------------------------------------- 
productFr :: [Integer] -> Integer
productFr xs = foldr (*) 1 xs

-- Задача 3 -----------------------------------------
concatFr :: [Int] -> [Int] -> [Int]
concatFr xs ys = foldr (:) ys xs  


-- Задача 4 -----------------------------------------
insert :: [Int] -> Int -> [Int]
insert xs v = [x | x <- xs, x < v] ++ [v] ++ [x | x <- xs, x >= v]

sortInsert :: [Int] -> [Int]
sortInsert xs = foldl (insert) [] xs

-- Задача 5 -----------------------------------------

getIndex :: (Int -> Bool) -> Int -> Int -> Int
getIndex p i x = let newI = (abs i) + 1 in (if p x then newI else -1 * newI)

findIndices ::(Int -> Bool) -> [Int] -> [Int]
findIndices p xs = [ x - 1 | x <- tail (scanl (getIndex p) 0 xs), x > 0 ]

-- Задача 6 -----------------------------------------
allReverse :: [String] -> [String]
allReverse xss = reverse (map reverse xss)

-- Задача 7  -----------------------------------------
noDigits :: String -> String
noDigits xs = filter (\x -> not (elem x ['0' .. '9'])) xs

-- Задача 8 ------------------------------------------
cntGood :: [Int -> Bool] -> Int -> Int
cntGood ps v = foldl (\sm p -> if p v then sm + 1 else sm) 0 ps

-- Задача 9 ------------------------------------------
trianglePas :: [[Integer]]
trianglePas = iterate (\xs -> zipWith (+) (0 : xs) (xs ++ [0])) [1]

-- Задача 10 -----------------------------------------
factorialsM :: [Integer]
factorialsM = 1 : zipWith (*) factorialsM [2 ..]