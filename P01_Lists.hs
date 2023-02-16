{-# OPTIONS_GHC -Wall #-}

module P01_Lists where

-- Задача 1 -----------------------------------------
power3 :: [Integer]
power3 = [x ^ (3 :: Integer) | x <- [1 ..] :: [Integer]]

-- Задача 2 -----------------------------------------
toPower3 :: [Integer]
toPower3 = [(3 :: Integer) ^ x | x <- [1 ..] :: [Integer]]

-- Задача 3 -----------------------------------------
sumList :: [Integer] -> Integer
sumList xs = if null xs then 0 else head xs + sumList (tail xs)

sumPower3 :: Integer -> Integer
sumPower3 n = sumList (take (fromIntegral n) toPower3)

-- Задача 4 -----------------------------------------
sumPower :: Integer -> Integer -> Integer
sumPower m n = sumList (take (fromIntegral n) [m ^ x | x <- [1 ..] :: [Integer]])

-- Задача 5 -----------------------------------------
lessMe :: [Int] -> [Int]
lessMe xs = map (\x -> length (filter (< x) xs)) xs

-- Задача 6 -----------------------------------------
hailstone :: Int -> Int
hailstone n = if even n then div n 2 else n * 3 + 1

-- Задача 7 -----------------------------------------

hailSeqHelper :: [Int] -> [Int]
hailSeqHelper xs = if head xs == 1 then xs else hailSeqHelper (hailstone (head xs) : xs)

hailSeq :: Int -> [Int]
hailSeq n = reverse (hailSeqHelper [n])

-- Задача 8 -----------------------------------------
allHailSeq :: [[Int]]
allHailSeq = [hailSeq x | x <- [1 ..]]

-- Задача 9 -----------------------------------------
find :: ([Int] -> Bool) -> [[Int]] -> [Int]
find p xs
  | null xs = []
  | p (head xs) = head xs
  | otherwise = find p (tail xs)

firstHailSeq :: Int -> Int
firstHailSeq n = head (find (\xs -> length xs == n) allHailSeq)
