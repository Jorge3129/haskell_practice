{-# OPTIONS_GHC -Wall #-}

module P04_Graphs where

import           Data.List

type GraphS = (Int, [(Int, Int)])

type Graph = [[Int]]

-- graph utils
nodes :: Graph -> [Int]
nodes g = [0 .. (length g - 1)]

goNodes :: Graph -> Int -> [Int]
goNodes gr v = snd $ until cond (oneStep gr) ([v], [])

cond :: ([Int], [Int]) -> Bool
cond (ns, _) = null ns

oneStep :: Graph -> ([Int], [Int]) -> ([Int], [Int])
oneStep gr (ns, os) =
  let old = ns ++ os
      ns1 = [n | v <- ns, n <- gr !! v]
      ns2 = filter (`notElem` old) ns1
      new = nub ns2
   in (new, old)

-- path utils
allPaths :: Graph -> Int -> [[[Int]]]
allPaths gr v = until condP (stepP gr) [[[v]]]

condP :: [[[Int]]] -> Bool
condP pss = null (head pss)

stepP :: Graph -> [[[Int]]] -> [[[Int]]]
stepP gr pss@(psn : _) =
  [t : p | p@(x : xs) <- psn, x `notElem` xs, t <- gr !! x] : pss
stepP _ [] = error "allPaths:stepP"

-- Задача 1 ------------------------------------
isOrdinary :: Graph -> Bool
isOrdinary gr = noLoops gr && noInvalidVertices gr && noHalfEdges gr

noLoops :: Graph -> Bool
noLoops gr = null [x | x <- zip [0 ..] gr, uncurry elem x]

noInvalidVertices :: Graph -> Bool
noInvalidVertices gr =
  let vertices = length gr
   in null [x | x <- gr, any (\v -> v < 0 || v >= vertices) x]

noHalfEdges :: Graph -> Bool
noHalfEdges gr =
  all
    ( \(cur, adj) ->
        all
          ( \v ->
              (length [y | y <- gr !! v, y == cur]) == 1
          )
          adj
    )
    (zip [0 ..] gr)

-- Задача 2 ------------------------------------
fromGraph :: Graph -> GraphS
fromGraph gr = (length gr - 1, [(fst x, y) | x <- zip [0 ..] gr, y <- snd x])

-- Задача 3 ------------------------------------
toGraph :: GraphS -> Graph
toGraph gr = [[snd y | y <- snd gr, fst y == x] | x <- [0 .. fst gr]]

-- Задача 4 ------------------------------------
shortWay :: Graph -> Int -> Int -> [Int]
shortWay gr a b =
  if a == b
    then []
    else
      let paths = [y | x <- allPaths gr a, y <- reverse x, head y == b]
       in if null paths
            then []
            else reverse (last paths)

-- Задача 5 ------------------------------------
isConnecting :: Graph -> Bool
isConnecting gr =
  null
    [ p | p <- (uniquePairs . nodes) gr, not (uncurry (isConnectedTo gr) p)
    ]

isConnectedTo :: Graph -> Int -> Int -> Bool
isConnectedTo gr a b = (not . null) (getAllPathsTo (allPaths gr a) b)

uniquePairs :: [Int] -> [(Int, Int)]
uniquePairs xs = [(x, y) | x <- xs, y <- xs, x < y]

-- Задача 6 ------------------------------------
components :: Graph -> [[Int]]
components gr =
  reverse
    ( foldl
        ( \xss v ->
            if v `elem` concat xss
              then xss
              else sort (goNodes gr v) : xss
        )
        []
        (nodes gr)
    )

-- Задача 7 ------------------------------------
eccentricity :: Graph -> Int -> Int
eccentricity gr a =
  let allPs = allPaths gr a
      vs = [x | x <- nodes gr, x /= a]
      ds = [distance allPs v | v <- vs]
   in maximum ds

distance :: [[[Int]]] -> Int -> Int
distance allPs b =
  let simplePs = [p | p <- getAllPathsTo allPs b, nub p == p]
      lns = [length p - 1 | p <- reverse simplePs]
   in head lns

getAllPathsTo :: [[[Int]]] -> Int -> [[Int]]
getAllPathsTo allPs b = [p | p <- concat [reverse ps | ps <- allPs], head p == b]

-- Задача 8 ------------------------------------
findDiameter :: Graph -> Int
findDiameter gr = maximum [eccentricity gr v | v <- nodes gr]

findRadius :: Graph -> Int
findRadius gr = minimum [eccentricity gr v | v <- nodes gr]

-- Задача 9 ------------------------------------
findCenter :: Graph -> [Int]
findCenter gr = [v | v <- nodes gr, isCentral gr v]

isCentral :: Graph -> Int -> Bool
isCentral gr a = eccentricity gr a == findRadius gr

-- Задача 10 ------------------------------------
shortWayAll :: Graph -> Int -> Int -> [[Int]]
shortWayAll gr a b =
  let pss = reverse (allPathsTo gr a b)
   in if null pss then [] else head pss

allPathsTo :: Graph -> Int -> Int -> [[[Int]]]
allPathsTo gr a b =
  let pss = [choosePathsTo ps b | ps <- allPaths gr a]
   in [ps | ps <- pss, not (null ps)]

choosePathsTo :: [[Int]] -> Int -> [[Int]]
choosePathsTo ps a = [reverse p | p <- ps, head p == a]

---------------------Тестові дані - Графи -------
gr1S, gr2S :: GraphS
gr1S =
  ( 5,
    [ (0, 1),
      (0, 2),
      (0, 3),
      (1, 0),
      (1, 3),
      (1, 4),
      (2, 0),
      (2, 4),
      (2, 5),
      (3, 0),
      (3, 1),
      (4, 1),
      (4, 2),
      (5, 2)
    ]
  )
gr2S =
  ( 7,
    [ (0, 1),
      (0, 3),
      (1, 0),
      (1, 2),
      (2, 1),
      (2, 3),
      (3, 0),
      (3, 2),
      (4, 5),
      (4, 6),
      (5, 4),
      (5, 6),
      (6, 4),
      (6, 5)
    ]
  )

gr1, gr2 :: Graph
gr1 = [[1, 2, 3], [0, 3, 4], [0, 4, 5], [0, 1], [1, 2], [2]]
gr2 = [[1, 3], [0, 2], [1, 3], [0, 2], [5, 6], [4, 6], [4, 5], []]
