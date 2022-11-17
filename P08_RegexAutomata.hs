{-# OPTIONS_GHC -Wall #-}
module P08_RegexAutomata where

import Data.List
import qualified Text.ParserCombinators.Parsec as P

data RE = Null   |
          Term Char |
          Seq RE RE |
          Alt RE RE |
          Rep RE    |
          Plus RE   |
          Opt RE
        deriving (Eq, Show)

type State = Int
data Label = C Char | Eps deriving (Eq, Ord, Show)
type Transition = (State, State, Label)
type Automation = (State, [State], [Transition])

type MetaState = [State]
type MetaTransition = (MetaState, MetaState, Label)

-- Задача 1 -----------------------------------------
simplify :: RE -> RE   
simplify Null = Null
simplify (Term ch) = Term ch
simplify (Seq rex1 rex2) = Seq (simplify rex1) (simplify rex2)
simplify (Alt rex1 rex2) = Alt (simplify rex1) (simplify rex2)
simplify (Rep re) = Rep (simplify re)
simplify (Plus re) = Seq rex1 (Rep rex1) where rex1 = simplify re
simplify (Opt re) = Alt rex1 Null where rex1 = simplify re

-- Задача 2 -----------------------------------------
isTerminal :: Automation -> State -> Bool 
isTerminal (_, sts, _) s = elem s sts

isEssential :: Automation -> State -> Bool 
isEssential aut@(_, _, trs) s =
  isTerminal aut s || 
  (not . null) [ tr | tr@(is, _, l) <- trs, is == s && isReadLabel l]

isReadLabel :: Label -> Bool
isReadLabel Eps = False
isReadLabel (C _) = True

-- Задача 3 -----------------------------------------
transitionsFrom :: Automation -> State -> [Transition]
transitionsFrom (_, _, trs) s = [ tr | tr@(is, _, _) <- trs, is == s]

-- Задача 4 -----------------------------------------
labels :: [Transition] -> [Label]
labels trs = nub [ l | (_, _, l) <- trs, isReadLabel l]

-- Задача 5 -----------------------------------------
acceptsDA :: Automation -> String -> Bool
acceptsDA aut@(is, _, trs) wd =
  let res = foldl (stepDA trs) (Just is) wd
  in case res of
    Nothing -> False
    (Just st) -> isTerminal aut st

stepDA :: [Transition] -> Maybe State -> Char -> Maybe State
stepDA trs mbst ch = case mbst of
  Nothing -> Nothing
  (Just st) -> if null nxs then Nothing else Just (head nxs)
    where nxs = [b | (a, b, l) <- trs, a == st && l == (C ch)]

-- Задача 6 -----------------------------------------
stStep  :: Automation -> State -> Label -> [State]
stStep (_, _, trs) st mc = nub [b | (a, b, l) <- trs, a == st && l == mc]

setStep :: Automation -> [State] -> Label -> [State]
setStep naut bs mc = (nub . concat) [ stStep naut b mc | b <- bs] 

closure :: Automation -> [State] -> [State]
closure naut ss =
  fst $ until (null . snd) step ([], ss)
    where 
      step (acc, bs) =
        let newSts = sort (setStep naut [b | b <- bs, notElem b acc] Eps)
            newAcc = (sort . nub) (acc ++ bs)
        in (newAcc, newSts)

-- Задача 7 -----------------------------------------
accepts :: Automation -> String -> Bool
accepts naut@(is, _, _) wd =
  let ssn = foldl step (closure naut [is]) wd
        where step bs ch = closure naut (setStep naut bs (C ch))
  in any (isTerminal naut) ssn

-- Задача 8 -----------------------------------------
makeNDA :: RE -> Automation
makeNDA re = (1, [2], sort transitionsAll)
  where (transitionsAll, _) = make (simplify re) 1 2 3

make :: RE -> Int -> Int -> Int -> ([Transition], Int) 
make Null beg fin nxt     = ([(beg, fin, Eps)], nxt)
make (Term c) beg fin nxt = ([(beg, fin, (C c))], nxt)
make (Seq r1 r2) m n k =
  let (mr1, nx1) = make r1 m k (k+2)
      (mr2, nx2) = make r2 (k+1) n nx1
  in (mr1 ++ mr2 ++ [(k, k+1, Eps)], nx2)

make (Alt r1 r2) m n k =
  let (mr1, nx1) = make r1 k (k+1) (k+4)
      (mr2, nx2) = make r2 (k+2) (k+3) nx1
  in ([(m, k, Eps), (m, k+2, Eps)] ++ mr1 ++ mr2 ++ [(k+1, n, Eps), (k+3, n, Eps)], nx2)

make (Rep r1) m n k =
  let (mr1, nx1) = make r1 k (k+1) (k+2)
  in ([(m, k, Eps), (m, n, Eps), (k+1, k, Eps), (k+1, n, Eps)] ++ mr1, nx1)

-- Задача 9 -----------------------------------------
parseReg :: String -> Maybe RE 
parseReg str =
  case P.parse reg "" str of
    (Left _) -> Nothing
    (Right x) -> Just x

rsymb :: P.Parser RE
rsymb = do 
  c <- P.noneOf "*+?|()"
  return (Term c)

prime :: P.Parser RE
prime = rsymb P.<|> paren rexpr

rfact :: P.Parser RE
rfact = do
  c <- prime
  ops <- P.many (P.oneOf "*+?")
  return (foldl (flip appOp) c ops)

appOp :: Char -> RE -> RE
appOp '*' = Rep
appOp '+' = Plus
appOp '?' = Opt
appOp _ = error "incorrect symbol"

rterm :: P.Parser RE
rterm = do
  (y:ys) <- P.many1 rfact 
  return (foldl Seq y ys)

rexpr :: P.Parser RE
rexpr = P.chainl1 rterm alt

reg :: P.Parser RE
reg = rexpr <* P.eof

alt :: P.Parser (RE -> RE -> RE)
alt = P.string "|" >> return Alt

paren :: P.Parser a -> P.Parser a
paren p = P.string "(" *> p <* P.string ")" 

-- Задача 10 -----------------------------------------


makeDA' :: Automation -> (MetaState, [MetaState], [MetaTransition])
makeDA' aut@(is,_,_) =
  let isx = getEssClos aut [is]
      (gsmx, _, mtrx) = until (\(_,bmsx,_) -> null bmsx) (makeDaStep aut) ([], [isx], [])
  in (isx, gsmx, mtrx)

type MakeDATuple = ([MetaState], [MetaState], [MetaTransition])

makeDaStep :: Automation -> MakeDATuple -> MakeDATuple
makeDaStep aut (gmsx, bmsx@(mn:mnx), mtrx) =
  let ls = getLabels aut mn
      mlxs= [((getEssClos aut) . (setStep aut mn)) l | l <- ls]
      nmtx = [ (mn, mlx, l) | (mlx, l) <- zip mlxs ls]
      nmx = [ s | s <- mlxs, notElem s (gmsx ++ bmsx)]
  in (mn:gmsx, mnx ++ nmx, mtrx ++ nmtx)

getEssClos :: Automation -> MetaState -> MetaState
getEssClos aut ms = [s | s <- closure aut ms, isEssential aut s]

getLabels :: Automation -> MetaState -> [Label]
getLabels aut st = concatMap (labels . (transitionsFrom aut)) st


makeDA :: Automation -> Automation
makeDA aut@(_, fsx, _) =
  let (isx, msx, mtx) = makeDA' aut
      rmsx = zip ((reverse . nub) msx) [1..]
      nisx = renum rmsx isx
      nmtx = [(renum rmsx a, renum rmsx b, c) | (a,b,c) <- (nub mtx)]
      fmsx = [n | (s, n) <- rmsx, any (\f -> elem f s) fsx]
  in (nisx, fmsx, sort nmtx)

renum :: [(MetaState, Int)]-> MetaState -> Int
renum rmsx ms = head [n | (s, n) <- rmsx, s == ms]
-------------------------------------------------------
-- showRE - Функція може бути корисною при тестуванні
showRE :: RE -> String
showRE (Seq re re') = showRE re ++ showRE re'
showRE (Alt re re') = "(" ++ showRE re ++ "|" ++ showRE re' ++ ")"
showRE (Rep re)     = showRE' re ++ "*"
showRE (Plus re)    = showRE' re ++ "+"
showRE (Opt re)     =  showRE' re ++ "?"
showRE re           = showRE' re

showRE' :: RE -> String
showRE' Null      = ""
showRE' (Term c)  = [c]
showRE' (Alt re re') = showRE (Alt re re')
showRE' re        = "(" ++ showRE re ++ ")"

--------------------------------------------------------
-- Тестові приклади
reFigureS, re1S, re2S, re3S, re4S, re5S, re6S :: String
reFigureS = "(a|b)*c"
re1S = "(x|y)(1|2)"
re2S = "x'*"
re3S = "(ab|c)*"
re4S = "(a?)a"
re5S = "(ab)?d+"
re6S = "c?*"

reFigure, re1, re2, re3, re4, re5, re6 :: RE
reFigure = Seq (Rep (Alt (Term 'a') (Term 'b'))) (Term 'c')
re1 = Seq (Alt (Term 'x') (Term 'y')) (Alt (Term '1') (Term '2'))
re2 = Seq (Term 'x') (Rep (Term '\''))
re3 = Rep (Alt (Seq (Term 'a') (Term 'b')) (Term 'c'))
re4 = Seq (Opt(Term 'a')) (Term 'a')
re5 = Seq (Opt (Seq (Term 'a') (Term 'b'))) (Plus (Term 'd'))
re6 = Rep (Opt (Term 'c'))

ndaFigure, nda1, nda2, nda3, nda4, nda5, nda6, ndaTest :: Automation
daFigure, da1, da2, da3, da4, da5, da6 :: Automation
ndaFigure
  = (1,[2],[(1,3,Eps),(1,5,Eps),(3,4,Eps),(4,2,C 'c'),(5,7,Eps),
            (5,9,Eps),(6,3,Eps),(6,5,Eps),(7,8,C 'a'),(8,6,Eps),
            (9,10,C 'b'),(10,6,Eps)])
daFigure
  = (1,[2],[(1,1,C 'a'),(1,1,C 'b'),(1,2,C 'c')])

nda1 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,9,Eps),(4,11,Eps),
            (5,6,C 'x'),(6,3,Eps),(7,8,C 'y'),(8,3,Eps),(9,10,C '1'),
            (10,2,Eps),(11,12,C '2'),(12,2,Eps)])
da1 = (1,[3],
     [(1,2,C 'x'),(1,2,C 'y'),(2,3,C '1'),(2,3,C '2')])

nda2 = (1,[2],[(1,3,C 'x'),(3,4,Eps),(4,2,Eps),(4,5,Eps),(5,6,C '\''),
            (6,2,Eps),(6,5,Eps)])
da2 = (1,[2],
     [(1,2,C 'x'),(2,2,C '\'')])

nda3 = (1,[2],[(1,2,Eps),(1,3,Eps),(3,5,Eps),(3,7,Eps),(4,2,Eps),
            (4,3,Eps), (5,9,C 'a'),(6,4,Eps),(7,8,C 'c'),(8,4,Eps),
            (9,10,Eps),(10,6,C 'b')])
da3 = (1,[1],
     [(1,1,C 'c'),(1,2,C 'a'),(2,1,C 'b')])

nda4 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,2,C 'a'),(5,6,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps)])
da4 = (1,[2,3],[(1,2,C 'a'),(2,3,C 'a')])

nda5 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,11,C 'd'),(5,9,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps),(9,10,Eps),(10,6,C 'b'),
            (11,12,Eps),(12,2,Eps),(12,13,Eps),(13,14,C 'd'),
            (14,2,Eps),(14,13,Eps)])
da5 = (1,[2],[(1,2,C 'd'),(1,3,C 'a'),(2,2,C 'd'),(3,4,C 'b'),
            (4,2,C 'd')])

nda6 = (1,[2], [(1,2,Eps),(1,3,Eps),(3,5,Eps),(5,6, C 'c'), (6,4,Eps), 
                (4,2,Eps), (3,7,Eps), (7,8,Eps), (8,4,Eps), (4,3,Eps)]) 
da6 = (1,[1], [(1,1, C 'c')])

ndaTest = (1, [1], [(1,2, C 'a'), (1,4, Eps), (1,3, C 'b'), (2,3, Eps),
              (3,5, Eps), (3,4, C 'a'), (4,4, Eps), (4,1, Eps), (5,2, Eps), (5,4,Eps)] )
