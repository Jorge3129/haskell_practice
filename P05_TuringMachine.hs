{-# OPTIONS_GHC -Wall #-}

module P05_TuringMachine where

import           Data.List (nub, sort)

-- машина Тюрінга
data Going = No | Lt | Rt deriving (Show, Eq, Ord)

type Table = [((Int, Char), (Int, Char, Going))]

type Machine = (Int, Table)

type Config = (String, (Int, Char), String, Int)

-- опис складної машини Тюрінга
-- L | R | P Char | G  базові машини
data Complex
  = Join [Complex]
  | While Char Complex
  | If Char Complex Complex
  | P Char
  | L
  | R
  | G
  deriving (Show, Eq)

-- Задача 1.a -----------------------------------------
alphabet :: Table -> String
alphabet t = (sort . nub . concat) [[a, b] | ((_, a), (_, b, _)) <- t]

-- Задача 1.b -----------------------------------------
states :: Machine -> [Int]
states (is, t) = sort . nub . concat $ [is] : [[x | x <- [a, b], x /= 0] | ((a, _), (b, _, _)) <- t]

-- Задача 2 -----------------------------------------
iswfMachine :: Machine -> Bool
iswfMachine m = hasAllCombinations m && statesAreCorrect m && initStateIsCorrect m

-- check duplicates
noDoubleKeys :: Machine -> Bool
noDoubleKeys m@(_, t) = length (keySet m) == length t

keySet :: Machine -> [(Int, Char)]
keySet (_, t) = nub [key | (key, _) <- t]

-- check all combinations
hasAllCombinations :: Machine -> Bool
hasAllCombinations m@(_, t) = noDoubleKeys m && length (allPossibleKeys m) == length t

allPossibleKeys :: Machine -> [(Int, Char)]
allPossibleKeys m@(_, t) =
  let as = alphabet t
      sts = states m
   in [(st, a) | st <- sts, a <- as]

-- check all states
statesAreCorrect :: Machine -> Bool
statesAreCorrect m = null [st | st <- getMainStates m, st <= 0]

getMainStates :: Machine -> [Int]
getMainStates (_, t) = nub [a | ((a, _), _) <- t]

-- check initial state
initStateIsCorrect :: Machine -> Bool
initStateIsCorrect m@(is, _) = is == maximum (getMainStates m)

initCon :: Machine -> String -> Config
-- будує початкову конфігурацію за машиною і вхідним рядком
initCon (is, _) ""       = ("", (is, ' '), "", 0)
initCon (is, _) (c : cx) = ("", (is, c), cx, 0)

-- Задача 3.a -----------------------------------------
isFinal :: Int -> Config -> Bool
isFinal mx (_, (st, _), _, n) = st == 0 || n >= mx

-- Задача 3.b -----------------------------------------
stepM :: Machine -> Config -> Config
stepM m (before, comb, after, stepCount) =
  let (_, (state, ch, go)) = findRule m comb
   in move go (before, (state, ch), after, stepCount + 1)

findRule :: Machine -> (Int, Char) -> ((Int, Char), (Int, Char, Going))
findRule (_, t) st = head [rule | rule@(key, _) <- t, key == st]

move :: Going -> Config -> Config
move No cf = cf
move Rt (before, (st, ch), after, n) =
  (before ++ [ch], (st, safeHead after), safeTail after, n)
move Lt (before, (st, ch), after, n) =
  let revBefore = reverse before
      start = reverse (safeTail revBefore)
      end = safeHead revBefore
   in (start, (st, end), ch : after, n)

safeHead :: String -> Char
safeHead str = if null str then ' ' else head str

safeTail :: String -> String
safeTail str = if null str then "" else tail str

-- Задача 4 -----------------------------------------
eval :: Machine -> Int -> String -> Maybe String
eval m mx u =
  let (before, (st, ch), after, _) = until (isFinal mx) (stepM m) (initCon m u)
   in if st == 0
        then Just $ trim (before ++ [ch] ++ after)
        else Nothing

eval1 :: Machine -> Int -> String -> [Config]
eval1 m mx u = take mx (iterate (stepM m) (initCon m u))

trim :: String -> String
trim = trimEnd . trimStart

trimStart :: String -> String
trimStart = until (\s -> null s || head s /= ' ') tail

trimEnd :: String -> String
trimEnd = reverse . trimStart . reverse

-- Задача 5.a -----------------------------------------
renum :: Int -> Machine -> Machine
renum k (is, t) =
  ( is + k,
    [ ((ks + k, kc), (safeRenum k vs, vc, vg))
      | ((ks, kc), (vs, vc, vg)) <- t
    ]
  )

safeRenum :: Int -> Int -> Int
safeRenum k s = if s == 0 then s else s + k

-- Задача 5.b -----------------------------------------
connect :: Int -> Table -> Table
connect p t =
  [ ((ks, kc), (conRenum p vs, vc, vg))
    | ((ks, kc), (vs, vc, vg)) <- t
  ]

conRenum :: Int -> Int -> Int
conRenum p s = if s == 0 then p else s

-- Задача 6.a -----------------------------------------
seqJoin :: Machine -> Machine -> Machine
seqJoin m1 (is2, go2) =
  let (isa, goa) = renum is2 m1
      gob = connect is2 goa
   in (isa, sort $ go2 ++ gob)

-- Задача 6.b -----------------------------------------
ifJoin :: Char -> String -> Machine -> Machine -> Machine
ifJoin c alf m1 (is2, go2) =
  let (isa, goa) = renum is2 m1
      is = isa + 1
      auxT = [((is, s), (if s == c then isa else is2, s, No)) | s <- alf]
      t = sort $ go2 ++ goa ++ auxT
   in (is, t)

-- Задача 6.c -----------------------------------------
cycleJoin :: Char -> String -> Machine -> Machine
cycleJoin c alf (is, go) =
  let is1 = is + 1
      go1 = connect is1 go
      auxT = [((is1, s), (if s == c then is else 0, s, No)) | s <- alf]
      t = go1 ++ auxT
   in (is1, t)

-- Задача 7 -----------------------------------------
build :: String -> Complex -> Machine
build alf R = moveRt alf
build alf L = moveLt alf
build alf G = moveNo alf
build alf (P c) = putC c alf
build alf (If c m1 m2) = ifJoin c alf (build alf m1) (build alf m2)
build alf (While c m) = cycleJoin c alf (build alf m)
build alf (Join ms) = foldl (\jm m -> seqJoin jm (build alf m)) (build alf (head ms)) (tail ms)

moveRt :: String -> Machine
moveRt alf = (1, [((1, s), (0, s, Rt)) | s <- alf])

moveLt :: String -> Machine
moveLt alf = (1, [((1, s), (0, s, Lt)) | s <- alf])

moveNo :: String -> Machine
moveNo alf = (1, [((1, s), (0, s, No)) | s <- alf])

putC :: Char -> String -> Machine
putC c alf = (1, [((1, s), (0, c, No)) | s <- alf])

-- Задача 8.a-----------------------------------------
subtractAbs :: Complex
subtractAbs =
  Join
    [ While '|' (Join [If '|' (P ' ') G, right, right, L, If '|' (P ' ') G, left, left, R]),
      If ' ' (Join [right, P '|']) (If '#' (P ' ') G)
    ]

-- Задача 8.b-----------------------------------------
subtraction :: Complex
subtraction =
  Join
    [ While '|' (Join [If '|' (P ' ') G, right, right, L, If '|' (P ' ') G, left, left, R]),
      If ' ' (Join [right, P '|']) (Join [P ' ', R, If '|' (While '|' G) G])
    ]

toNotation :: Int -> String
toNotation n = replicate n '|'

sub :: Int -> Int -> Maybe Int
sub a b = maybeLen (eval (build " #|" subtraction) 200 (toNotation a ++ "#" ++ toNotation b))

subAbs :: Int -> Int -> Maybe Int
subAbs a b = maybeLen (eval (build " #|" subtractAbs) 200 (toNotation a ++ "#" ++ toNotation b))

maybeLen :: Maybe String -> Maybe Int
maybeLen (Just a) = Just (length a)
maybeLen Nothing  = Nothing

--------------------------------------------------------
--  тестові дані
-- приклади машин Тюрінга
test1, test2 :: Machine
-- алфавіт " abc": знаходить перший символ 'a' заміняє на 'b' і зупиняється
test1 =
  ( 1,
    [ ((1, 'a'), (0, 'b', No)),
      ((1, 'b'), (1, 'b', Rt)),
      ((1, 'c'), (1, 'c', Rt)),
      ((1, ' '), (1, ' ', Rt))
    ]
  )
-- алфавіт " a": невизначена функція переходу в (1,'a') !!!
test2 = (2, [((2, 'a'), (2, 'a', Rt)), ((2, ' '), (1, ' ', Rt)), ((1, ' '), (0, ' ', Rt))])

-- будуємо складну машину з найпростіших
-- будуємо машину, що обчислює додавання
-- найпростіші . алфавіт == " #|"
rht, putO, putW :: Machine
rht = (1, map (\c -> ((1, c), (0, c, Rt))) " #|") -- переміщує вправо
putO = (1, map (\c -> ((1, c), (0, '|', No))) " #|") -- записує символ '|'
putW = (1, map (\c -> ((1, c), (0, ' ', No))) " #|") -- записує символ ' '

-- складніші машини
rightO, rightM, main, additionM :: Machine
rightO = cycleJoin '|' " #|" rht -- проходить вправо всі '|'
rightM = seqJoin rht rightO -- вправо завжди і потім вправо всі '|'
main = seqJoin (seqJoin putW rightM) putO -- додавання, коли x>0
additionM = ifJoin '|' " #|" main putW -- додавання, коли x>=0

-- приклади побудов машин Тюрінга (обєкти типу Complex)
right, left, copy, addition :: Complex
-- вправо завжди і потім вправо всі '|'
right = Join [R, While '|' R]
-- вліво завжди і потім вліво всі '|'
left = Join [L, While '|' L]
-- додавання x+y
addition = If '|' (Join [P ' ', right, P '|']) (P ' ')
-- копіювання *|.x.| |.y.| ==> *|.x.| |.y+x.|
copy =
  Join
    [ While '|' (Join [P ' ', right, right, P '|', left, left, P '|', R]),
      Join [left, R]
    ]

rightOT, rightMT, mainT, additionMT :: Machine
rightOT =
  ( 2,
    [ ((1, ' '), (2, ' ', Rt)),
      ((1, '#'), (2, '#', Rt)),
      ((1, '|'), (2, '|', Rt)),
      ((2, ' '), (0, ' ', No)),
      ((2, '#'), (0, '#', No)),
      ((2, '|'), (1, '|', No))
    ]
  )
rightMT =
  ( 3,
    [ ((1, ' '), (2, ' ', Rt)),
      ((1, '#'), (2, '#', Rt)),
      ((1, '|'), (2, '|', Rt)),
      ((2, ' '), (0, ' ', No)),
      ((2, '#'), (0, '#', No)),
      ((2, '|'), (1, '|', No)),
      ((3, ' '), (2, ' ', Rt)),
      ((3, '#'), (2, '#', Rt)),
      ((3, '|'), (2, '|', Rt))
    ]
  )
mainT =
  ( 5,
    [ ((1, ' '), (0, '|', No)),
      ((1, '#'), (0, '|', No)),
      ((1, '|'), (0, '|', No)),
      ((2, ' '), (3, ' ', Rt)),
      ((2, '#'), (3, '#', Rt)),
      ((2, '|'), (3, '|', Rt)),
      ((3, ' '), (1, ' ', No)),
      ((3, '#'), (1, '#', No)),
      ((3, '|'), (2, '|', No)),
      ((4, ' '), (3, ' ', Rt)),
      ((4, '#'), (3, '#', Rt)),
      ((4, '|'), (3, '|', Rt)),
      ((5, ' '), (4, ' ', No)),
      ((5, '#'), (4, ' ', No)),
      ((5, '|'), (4, ' ', No))
    ]
  )
additionMT =
  ( 7,
    [ ((1, ' '), (0, ' ', No)),
      ((1, '#'), (0, ' ', No)),
      ((1, '|'), (0, ' ', No)),
      ((2, ' '), (0, '|', No)),
      ((2, '#'), (0, '|', No)),
      ((2, '|'), (0, '|', No)),
      ((3, ' '), (4, ' ', Rt)),
      ((3, '#'), (4, '#', Rt)),
      ((3, '|'), (4, '|', Rt)),
      ((4, ' '), (2, ' ', No)),
      ((4, '#'), (2, '#', No)),
      ((4, '|'), (3, '|', No)),
      ((5, ' '), (4, ' ', Rt)),
      ((5, '#'), (4, '#', Rt)),
      ((5, '|'), (4, '|', Rt)),
      ((6, ' '), (5, ' ', No)),
      ((6, '#'), (5, ' ', No)),
      ((6, '|'), (5, ' ', No)),
      ((7, ' '), (1, ' ', No)),
      ((7, '#'), (1, '#', No)),
      ((7, '|'), (6, '|', No))
    ]
  )
