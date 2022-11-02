{-# OPTIONS_GHC -Wall #-}
module P03_ContextFreeGrammars where

import Data.List

type Grammar = [Production]
type Production = (Char,String)
-- Граматика - список продукцій.
--   нетермінал першої - початковий

-- Лівосторонній вивід - послідовність слів  [String] з іншого боку
--     послідовність номерів правил, які при цьому застосовувались [Int]
type DerivationS = [String] 
type DerivationR = [Int] 


isNonTerm :: Char -> Bool
isNonTerm x = elem x ['A' .. 'Z']

isTerm :: Char -> Bool
isTerm x = not (isNonTerm x) && x /= '$'


-- Задача 1 -----------------------------------------
isGrammar :: Grammar -> Bool
isGrammar xs = foldl (\res p -> res && isNonTerm (fst p)) True xs


-- Задача 2.a ---------------------------------------
allTerm :: Grammar -> String
allTerm ps = nub (foldl (\res p -> res ++ getTerms p) "" ps)

getTerms :: Production -> String
getTerms p = [x | x <- snd p, isTerm x]


-- Задача 2.b ---------------------------------------
allNotT :: Grammar -> String
allNotT ps = nub (foldl (\res p -> res ++ getNonTerms p) "" ps)

getNonTerms :: Production -> String
getNonTerms p = fst p : [x | x <- snd p, isNonTerm x]


-- Задача 2.c ---------------------------------------
newMinN :: Grammar -> Char
newMinN ps = head [x | x <- ['A' .. 'Z'], notElem x (allNotT ps)]


-- Задача 3.a -----------------------------------------
buildGen :: Grammar -> String
buildGen ps = getAllGens ps ""

-- loop 'getGensOfKnown' until no new generating non-terminals are found
getAllGens :: Grammar -> String -> String
getAllGens ps knownGens = 
  let newGens = getGensOfKnown ps knownGens 
      allGens = newGens ++ knownGens
  in if null newGens 
    then allGens
    else getAllGens ps allGens

-- get all non-terminals that can generate 
-- the already known generating non-terminals
getGensOfKnown :: Grammar -> String -> String
getGensOfKnown ps [] = [fst p | p <- ps, all isTerm (snd p)]
getGensOfKnown ps knownGens =
  [ x | x <- 
  nub [fst p | p <- ps, hasSomeElemsFrom (snd p) knownGens],
  notElem x knownGens]

hasSomeElemsFrom :: String -> String -> Bool
hasSomeElemsFrom xs ys = any (\x -> elem x ys) xs


-- Задача 3.b -----------------------------------------
buildAcc :: Grammar -> String
buildAcc ps = getAllAccs ps [fst (head ps)]

-- loop 'getAccsFromKnown' until no new accessible non-terminals are found
getAllAccs :: Grammar -> String -> String
getAllAccs ps knownAccs = 
  let newAccs = getAccsFromKnown ps knownAccs 
      allAccs = newAccs ++ knownAccs
  in if null newAccs 
    then allAccs
    else getAllAccs ps allAccs

-- get all non-terminals that can be derived from
-- the already known accessible non-terminals
getAccsFromKnown :: Grammar -> String -> String
getAccsFromKnown ps knownAccs = 
  [ x | x <- 
  (nub . concat) [getRightNonTs p | p <- ps, elem (fst p) knownAccs],
  notElem x knownAccs]

-- get all non-terminals in the right-hand side of the production rule
getRightNonTs :: Production -> String
getRightNonTs p = [x | x <- snd p, isNonTerm x]


-- Задача 3.c -----------------------------------------
reduce :: Grammar -> Grammar
reduce ps = 
  let gens = buildGen ps
      accs = buildAcc ps
  in [p | p <- ps, let n = fst p, elem n gens, elem n accs]


-- Задача 4.a -----------------------------------------
findLeftR :: Grammar -> String
findLeftR ps =
  let candidateNonTs = [fst p | p <- ps, fstLeftRCondition p]
  in [x | x <- candidateNonTs, sndLeftRCondition x ps]

fstLeftRCondition :: Production -> Bool
fstLeftRCondition p = fst p == head (snd p)

sndLeftRCondition :: Char -> Grammar -> Bool
sndLeftRCondition x ps = any (\p -> (fst p) == x && not (fstLeftRCondition p)) ps


-- Задача 4.b -----------------------------------------
deleteLeftR :: Grammar -> Char -> Grammar
deleteLeftR ps nonTerm = 
  if notElem nonTerm (findLeftR ps) then ps
  else 
    let newNonTerm = newMinN ps
        startNT = fst (head ps)
    in restoreStartNonT startNT [refactProd nonTerm newNonTerm p | p <- ps] ++ [(newNonTerm, "")]

refactProd :: Char -> Char -> Production -> Production
refactProd nonTerm newNonTerm p =
  if fst p /= nonTerm then p else
  if fstLeftRCondition p
    then (newNonTerm, tail (snd p) ++ [newNonTerm])
    else (fst p, snd p ++ [newNonTerm])

restoreStartNonT :: Char -> Grammar -> Grammar
restoreStartNonT nonTerm gr =
  if fst (head gr) == nonTerm then gr
  else
    let withIndex = head [p | p <- zip gr [0..], fst (fst p) == nonTerm]
        rule = fst withIndex
        index = snd withIndex
    in [rule] ++ (take index gr) ++ (drop (index + 1) gr)


-- Задача 5.a -----------------------------------------
isFact :: Grammar -> Char -> Bool
isFact ps n = 
  let psFromN = [p | p <- ps, fst p == n]
  in foldl (\res p -> 
    res || any (\x -> snd x /= snd p && isCommonPrefix (snd x) (snd p)) psFromN
  ) False psFromN

isCommonPrefix :: String -> String -> Bool
isCommonPrefix xs ys = 
  let zipped = zipWith (\x y -> x == y) xs ys
  in if null zipped then False else head zipped


-- Задача 5.b -----------------------------------------
deleteFact :: Char -> String -> Grammar -> Grammar
deleteFact n prefix gr = 
  if not (isFact gr n) then gr
  else 
    let newNonTerm = newMinN gr
    in (n, prefix ++ [newNonTerm]) : [refactProd2 n newNonTerm prefix p | p <- gr]

refactProd2 :: Char -> Char -> String -> Production -> Production
refactProd2 nonTerm newNonTerm prefix p =
  if fst p /= nonTerm || not (startsWith prefix (snd p)) 
    then p
    else (newNonTerm, rmPrefix prefix (snd p))

startsWith :: String -> String -> Bool
startsWith xs ys = 
  let zipped = zipWith (\x y -> x == y) xs ys
  in length [x | x <- zipped, x] == length xs

rmPrefix :: String -> String -> String
rmPrefix prefix str = drop (length prefix) str


-- Задача 6.a -----------------------------------------
isLeftDerivationS :: Grammar -> DerivationS -> Bool
isLeftDerivationS gr sx = foldl (\res (start, end) -> 
    res && any (\p -> derive p start == end) gr
  ) True (zipDerivation sx)

derive :: Production -> String -> String
derive p xs =
  let (nonTerm, index) = zipIndexOfFstNonT xs
      derived = if fst p == nonTerm then snd p else [nonTerm]
  in (take index xs) ++ derived ++ (drop (index + 1) xs)

zipIndexOfFstNonT :: String -> (Char, Int)
zipIndexOfFstNonT xs = head [y | y <- zip xs [0 .. ], isNonTerm (fst y)]

zipDerivation :: DerivationS -> [(String, String)]
zipDerivation sx = zip (take (length sx - 1) sx) (tail sx)


-- Задача 6.b -----------------------------------------
isLeftDerivationR :: Grammar -> DerivationR -> Bool
isLeftDerivationR gr ix = fst (foldl (\acc@(success, expr) ruleIndex -> 
    let rule = gr !! ruleIndex
    in if not success then acc else
      let derived = derive rule expr 
      in (derived /= expr, derived)
  ) (True, getStart gr) ix)

getStart :: Grammar -> String
getStart gr =[fst (head gr)]


-- Задача 7 -----------------------------------------
fromLeftR :: Grammar -> DerivationR -> DerivationS
fromLeftR gr ix = scanl (\expr ruleIndex ->
  let rule = gr !! ruleIndex
  in derive rule expr
  ) (getStart gr) ix

--------------------------------------------------------
--  тестові дані 
gr0, gr1, gr1e, gr2 :: Grammar   
gr0 = [('S',"aAS"), ('S',"a"),('A',"SbA"),('A',"ba")]
gr1 = [ ('S',"aSa"), ('S',"bSd"), ('S',"c"), ('S',"aSb"), ('D',"aC") 
      , ('A',"cBd"), ('A',"aAd"),('B',"dAf"),('C',"cS"), ('C',"a")]
gr1e = [('S',"aAS"), ('S',"a"),('a',"SbA"),('A',"ba"),('S',"")]
gr2 = [('E',"E+T"),('E',"T"), ('T',"T*F"), ('T',"F"), ('F',"d"),('F',"(E)") ]   

gr0S, gr0Se :: [String]
gr0S =  ["S", "aAS", "aSbAS", "aabAS", "aabbaS", "aabbaa"]
gr0Se = ["S", "aAS", "aSbAS", "aabAS", "aabbaS", "aabba"]

gr0R, gr0Re:: DerivationR
gr0R = [0, 2, 1, 3, 1]
gr0Re = [0, 0, 1, 3, 1]



