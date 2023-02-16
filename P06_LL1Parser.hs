{-# OPTIONS_GHC -Wall #-}

module P06_LL1Parser where

import           Data.List
import           Data.Maybe

type Grammar = [Production] -- КВ-граматика

type Production = (Char, String) -- Правило виводу

type Predict = [(Char, String)] -- Прогнозуюча таблиця

type Control = [((Char, Char), Int)] -- Управляюча таблиця

-- Задача 1 ------------------------------------
addOne :: String -> Char -> String
addOne st c = if c `elem` st then st else st ++ [c]

addAll :: String -> String -> String
addAll = foldl addOne

addWithout :: String -> String -> String
addWithout st wd = sort (addAll st [c | c <- wd, c /= '$'])

inter :: String -> String -> String
inter st1 st2 = nub [c | c <- st1, c `elem` st2]

-- Задача 2 ------------------------------------
tkPredict :: Predict -> Char -> String
tkPredict pt n = safeHead "" [out | (nt, out) <- pt, nt == n]

upPredict :: Predict -> Char -> String -> Predict
upPredict pt n st = sort . nub $ [(nt, out) | (nt, out) <- pt, nt /= n] ++ [(n, st)]

safeHead :: a -> [a] -> a
safeHead dft xs = if null xs then dft else head xs

-- Задача 3 ------------------------------------
parse :: Grammar -> Control -> String -> Maybe [Int]
parse gr ctl st =
  let start = (fst . head) gr
      input = st ++ "$"
      predicate (_, stack, res) = null stack || isNothing res
   in let (_, _, result) = until predicate (step gr ctl) (input, [start, '$'], Just [])
       in result

step ::
  Grammar ->
  Control ->
  (String, String, Maybe [Int]) ->
  (String, String, Maybe [Int])
step gr ctl (input, stack, result) =
  let top = head stack
      ch = head input
   in if isNonTerm top
        then
          let pIx = [b | (a, b) <- ctl, a == (top, ch)]
           in if null pIx
                then (input, stack, Nothing)
                else
                  let (_, out) = gr !! head pIx
                   in (input, out ++ tail stack, (\xs -> Just (xs ++ [head pIx])) =<< result)
        else
          if top == ch
            then (tail input, tail stack, result)
            else (input, stack, Nothing)

isNonTerm :: Char -> Bool
isNonTerm x = x `elem` ['A' .. 'Z']

isTerm :: Char -> Bool
isTerm x = not (isNonTerm x) && x /= '$'

-- Задача 4 ------------------------------------
first :: Predict -> String -> String
first pFst st
  | null st = "$"
  | (isTerm . head) st = [head st]
  | otherwise =
      let fstA = findFst pFst (head st)
       in if length st == 1 || notElem '$' fstA
            then fstA
            else addWithout (first pFst (tail st)) fstA

findFst :: Predict -> Char -> String
findFst pr n = (snd . head) [p | p@(s, _) <- pr, s == n]

-- Задача 5 ------------------------------------
buildingControl :: Grammar -> Predict -> Predict -> Control
buildingControl gr pFst pNxt = sort $ foldl (ctlStep pFst pNxt) [] (zip gr [0 ..])

ctlStep :: Predict -> Predict -> Control -> (Production, Int) -> Control
ctlStep pFst pNxt acc ((nt, out), i) =
  let fstNt = first pFst out
      res = [((nt, a), i) | a <- fstNt, isTerm a]
   in if '$' `elem` fstNt
        then acc ++ res ++ [((nt, b), i) | b <- findFst pNxt nt]
        else acc ++ res

-- Задача 6 ------------------------------------
testingLL1 :: Grammar -> Predict -> Predict -> Bool
testingLL1 gr pFst pNxt =
  all
    ( \(n, rls) ->
        testFst pFst rls && testFollow pFst (findFst pNxt n) rls
    )
    (fromGrammar gr)

fromGrammar :: Grammar -> [(Char, [String])]
fromGrammar gr = [(n, [out | (m, out) <- gr, m == n]) | n <- nub [st | (st, _) <- gr]]

testFst :: Predict -> [String] -> Bool
testFst pFst rls =
  let ffs = zip [first pFst r | r <- rls] ([0 ..] :: [Int])
   in all null [inter x y | (x, xi) <- ffs, (y, yi) <- ffs, xi < yi]

testFollow :: Predict -> String -> [String] -> Bool
testFollow pFst nx rls =
  let notNulRls = [r | r <- rls, (not . null) r]
   in length notNulRls == length rls
        || all null [inter nx f | f <- [first pFst r | r <- notNulRls]]

-- Задача 7 ------------------------------------
buildFst :: Grammar -> Predict
buildFst = undefined

evalFst :: Grammar -> Predict -> Predict
evalFst = undefined

-- evalFst gr fstN =
--   if null fstN
--   then [ (n, if elem "" rls then "$" else "" ) | (n, rls) <- fromGrammar gr]
-- else
--   foldl (\fst (n, rls) ->
--     [ addWithout (getFst fstN n) (head r)
--       | r <- rls]
--   ) fstN (fromGrammar gr)

-- getFst :: Predict -> Char -> String
-- getFst pr c = if isTerm c then (c:[]) else head [ s | (n, s) <- pr, n == c]

extandFst :: Predict -> Production -> Predict
extandFst = undefined

-- Задача 8 ------------------------------------
buildNxt :: Grammar -> Predict -> Predict
buildNxt = undefined

nontermTails :: Grammar -> [(Char, String)]
nontermTails = undefined

evalNxt :: [(Char, String)] -> Predict -> Predict -> Predict
evalNxt = undefined

extandNxtOne :: Predict -> Char -> Predict -> String -> Predict
extandNxtOne = undefined

---------------------Тестові дані ---------------------------
gr6 :: Grammar
gr6 = [('S', "F"), ('S', "(S+F)"), ('F', "a")]

ctl6 :: Control
ctl6 = [(('S', '('), 1), (('S', 'a'), 0), (('F', 'a'), 2)]

gr0, gr1, gr2, gr3, gr4, gr5 :: Grammar
--  LL(1)-граматики
gr0 = [('S', "aAS"), ('S', "b"), ('A', "a"), ('A', "bSA")]
gr1 = [('S', "TV"), ('T', "d"), ('T', "(S)"), ('V', "+TV"), ('V', "-TV"), ('V', "")]
gr2 =
  [ ('E', "TU"),
    ('U', ""),
    ('U', "+TU"),
    ('U', "-TU"),
    ('T', "FV"),
    ('V', ""),
    ('V', "*FV"),
    ('V', "%FV"),
    ('V', "/FV"),
    ('F', "d"),
    ('F', "(E)")
  ]
-- не LL(1)-граматики
gr3 = [('S', "aAS"), ('S', "a"), ('A', "SbA"), ('A', "ba"), ('S', "")]
gr4 = [('E', "E+T"), ('E', "T"), ('T', "T*F"), ('T', "F"), ('F', "d"), ('F', "(E)")]
gr5 =
  [ ('E', "E+T"),
    ('E', "E-T"),
    ('E', "T"),
    ('T', "T*F"),
    ('T', "T%F"),
    ('T', "T/F"),
    ('T', "F"),
    ('F', "d"),
    ('F', "(E)")
  ]

-- прогнозуючі таблиці початкових терміналів Fst
pFst0, pFst1, pFst2, pFst3, pFst4, pFst5 :: Predict
pFst0 = [('A', "ab"), ('S', "ab")]
pFst1 = [('S', "(d"), ('T', "(d"), ('V', "$+-")]
pFst2 = [('E', "(d"), ('F', "(d"), ('T', "(d"), ('U', "$+-"), ('V', "$%*/")]
pFst3 = [('A', "ab"), ('S', "$a")]
pFst4 = [('E', "(d"), ('F', "(d"), ('T', "(d")]
pFst5 = [('E', "(d"), ('F', "(d"), ('T', "(d")]

-- прогнозуючі таблиці наступних терміналів Nxt
pNxt0, pNxt1, pNxt2, pNxt3, pNxt4, pNxt5 :: Predict
pNxt0 = [('A', "ab"), ('S', "$ab")]
pNxt1 = [('S', "$)"), ('T', "$)+-"), ('V', "$)")]
pNxt2 = [('E', "$)"), ('F', "$%)*+-/"), ('T', "$)+-"), ('U', "$)"), ('V', "$)+-")]
pNxt3 = [('A', "$ab"), ('S', "$b")]
pNxt4 = [('E', "$)+"), ('F', "$)*+"), ('T', "$)*+")]
pNxt5 = [('E', "$)+-"), ('F', "$%)*+-/"), ('T', "$%)*+-/")]

-- управляючі таблиці
ctl0, ctl1, ctl2 :: Control
ctl0 = [(('A', 'a'), 2), (('A', 'b'), 3), (('S', 'a'), 0), (('S', 'b'), 1)]
ctl1 =
  [ (('S', '('), 0),
    (('S', 'd'), 0),
    (('T', '('), 2),
    (('T', 'd'), 1),
    (('V', '$'), 5),
    (('V', ')'), 5),
    (('V', '+'), 3),
    (('V', '-'), 4)
  ]
ctl2 =
  [ (('E', '('), 0),
    (('E', 'd'), 0),
    (('F', '('), 10),
    (('F', 'd'), 9),
    (('T', '('), 4),
    (('T', 'd'), 4),
    (('U', '$'), 1),
    (('U', ')'), 1),
    (('U', '+'), 2),
    (('U', '-'), 3),
    (('V', '$'), 5),
    (('V', '%'), 7),
    (('V', ')'), 5),
    (('V', '*'), 6),
    (('V', '+'), 5),
    (('V', '-'), 5),
    (('V', '/'), 8)
  ]
