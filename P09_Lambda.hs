{-# OPTIONS_GHC -Wall #-}
module P09_Lambda where

import Text.ParserCombinators.Parsec
import Data.Char (isDigit)

data Term   =  Nmb Int         -- десяткове число без знаку
            | Var String       -- змінна, довільний ідентифікатор
            | App Term Term    -- операція застосування
            | Abs String Term  --  операція абстракції
           deriving (Show, Eq) 
type Contex = [(String,Term)]

-- utils
insert :: (Ord a) => [a] -> a -> [a]
insert xs v = [x | x <- xs, x < v] ++ [v] ++ [x | x <- xs, x >= v]

sort :: (Ord a) =>  [a] -> [a]
sort xs = foldl (insert) [] xs

nub' :: (Eq a) => [a] -> [a]
nub' [] = []
nub' (x:xs) = x : nub' (filter (/=x) xs)

notNull :: [a] -> Bool
notNull = not . null

-- Задача 1.a -----------------------------------------
addVar :: String -> [String] -> [String] 
addVar x xs = if elem x xs then xs else x:xs

-- Задача 1.b ----------------------------------------- 
delVar :: String -> [String] -> [String]
delVar x ys = [y | y <- ys, y /= x] 

-- Задача 1.c -----------------------------------------
unionV :: [String] -> [String] -> [String]
unionV xs ys = sort $ foldl (flip addVar) xs ys 

-- Задача 1.d ----------------------------------------- 
freeVars :: Term -> [String]
freeVars (Nmb _) = []  
freeVars (Var x) = [x]  
freeVars (App t1 t2) = unionV (freeVars t1) (freeVars t2)
freeVars (Abs x t) = [y | y <- freeVars t, y /= x]  

-- Задача 2.a -----------------------------------------
deleteSyn :: String -> Contex -> Contex
deleteSyn nm cnt = [s | s@(k, _) <- cnt, k /= nm] 

-- Задача 2.b -----------------------------------------
iswfTerm :: Term -> Contex -> Bool 
iswfTerm t cnt =
  null fvs || 
  all (\var -> 
    elem var (syns cnt)
    || isDigitNm var
  ) fvs
  where fvs = freeVars t

syns :: Contex -> [String]
syns = map fst

isDigitNm :: String -> Bool
isDigitNm s = notNull s && all isDigit s

-- Задача 2.c -----------------------------------------
iswfContex :: Contex -> Bool
iswfContex ctx =
  let sns = syns ctx
      pfx = prefixes ctx
  in nub' sns == sns && 
     foldl (\res pref -> 
      if not res then False 
      else checkLastTerm pref
     ) True pfx

checkLastTerm :: Contex -> Bool
checkLastTerm ctx =
  let ((_,t):xs) = reverse ctx
      pref = reverse xs
  in iswfTerm t pref

prefixes :: [a] -> [[a]]
prefixes xs = drop 1 (scanl (\acc y -> acc ++ [y]) [] xs)

-- Задача 3.a -----------------------------------------
isNumber :: Term -> Bool
isNumber t =
  case getDeclarations t of
    Nothing -> False
    (Just (s,z,t1)) -> expectApp (s,z) t1

getDeclarations :: Term -> Maybe (String, String, Term)
getDeclarations t = do
  s <- expectAbs t
  z <- expectAbs (snd s)
  return (fst s, fst z, snd z)

expectAbs :: Term -> Maybe (String, Term)
expectAbs (Abs x t) = Just (x,t)
expectAbs _ = Nothing

expectApp :: (String, String) -> Term -> Bool
expectApp (_, z) (Var x) = x == z
expectApp (s, z) (App (Var x) t) = x == s && expectApp (s, z) t
expectApp _ _ = False

-- Задача 3.b -----------------------------------------
inNumber :: Term -> Term
inNumber t =
   case getDeclarations t of
    Nothing -> notANum
    (Just (s,z,t1)) -> Nmb (evalNum (s,z) t1)

notANum :: a
notANum = error "not a number"

evalNum :: (String, String) -> Term -> Int
evalNum (_, z) (Var x) = if x == z then 0 else notANum
evalNum (s, z) (App (Var x) t) = if x == s then 1 + evalNum (s,z) t else notANum
evalNum _ _ = notANum

-- Задача 3.c -----------------------------------------
compress :: Term -> Term
compress (App t1 t2) = App (compress t1) (compress t2)
compress t@(Abs x t1) = if isNumber t then inNumber t else Abs x (compress t1)
compress t = t

-- Задача 4 -----------------------------------------
reduce :: Term -> String -> Term -> Term 
reduce (Var y) x s = if y == x then s else (Var y)
reduce (App t1 t2) x s = App (reduce t1 x s) (reduce t2 x s)
reduce (Abs y t) x s
  | x == y = (Abs y t)
  | notElem y (freeVars s) = (Abs y (reduce t x s))
  | otherwise =
    let fvs = unionV (freeVars s) (freeVars t)
        z = newVar fvs y
    in Abs z (reduce (reduce t y (Var z)) x s)
reduce t _ _ = t

-- Задача 5 -----------------------------------------
evalStep :: Term -> Contex -> Maybe Term
evalStep (Nmb x) _ = Just (integerTerm x)
evalStep (Var x) ctx = maybe Nothing Just (valFromCtx ctx x)
evalStep (App (Abs x b) t) _ = Just (reduce b x t)
evalStep (App t1 t2) ctx =
  let r1 = evalStep t1 ctx
  in case r1 of
    Nothing -> maybe Nothing (\x -> Just (App t1 x)) (evalStep t2 ctx)
    (Just tr1) -> Just (App tr1 t2)

evalStep (Abs x t) ctx = maybe Nothing (Just . (Abs x)) (evalStep t (deleteSyn x ctx))

valFromCtx :: Contex -> String -> Maybe Term
valFromCtx ctx nm = 
  let ls = [v | (k, v) <- ctx, k == nm] 
  in if null ls then Nothing else Just (head ls)

-- Задача 6 -----------------------------------------
eval :: Int -> Term -> Contex -> Maybe Term 
eval mx t ctx =
  let 
    step (cnt, (Just t1)) = (cnt + 1, maybe (Just t1) Just (evalStep t1 ctx))
    step t1 = t1
    cond (cnt, (Just t1)) = (not . (`canEval` ctx)) t1 || cnt >= mx
    cond _ = True
    (_, res) = until cond step (1, Just t)
  in maybe Nothing (\x -> 
    if canEval x ctx then Nothing
      else Just (compress x)) res

canEval :: Term -> Contex -> Bool
canEval (Nmb _) _ = True
canEval (Var x) ctx = valFromCtx ctx x /= Nothing
canEval (App (Abs _ _) _) _ = True
canEval (App t1 t2) ctx = canEval t1 ctx || canEval t2 ctx
canEval (Abs x t) ctx = canEval t (deleteSyn x ctx)

-- Задача 7 -----------------------------------------
parseTerm :: String -> Maybe Term 
parseTerm str =
  case parse expr "" str of
    (Left _) -> Nothing
    (Right x) -> Just x     

idf :: Parser String
idf = do
  st <- oneOf latinAlf
  nx <- many (oneOf alphNum)
  return (st:nx)

idp :: Parser Term
idp = do
  st <- idf
  return (Var st)

numb :: Parser Term
numb = do
  st <- read <$> (many1 digit)
  return (Nmb st)

func :: Parser Term
func = do
  reserved "\\"
  nms <- many1 (lexem idf)
  reserved "."
  t <- term
  return (foldl (flip Abs) t (reverse nms))

fact :: Parser Term
fact = parens term <|> lexem idp <|> lexem numb <|> func

term :: Parser Term
term = do 
  fx <- many1 fact
  return (
    if length fx == 1 then head fx else
      let (y:ys) = fx
      in foldl App y ys)

expr :: Parser Term
expr = do { _ <- spaces; v <- term; eof;  return v}

lexem :: Parser a -> Parser a
lexem p = do 
  a <- p
  spaces 
  return a

reserved :: String -> Parser ()
reserved s = do { _ <- string s; spaces} 

parens :: Parser a -> Parser a
parens p = do {reserved "("; n <- lexem p; reserved ")"; return n} 

latinAlf :: String
latinAlf = ['A'..'Z'] ++ ['a'..'z']

alphNum :: String
alphNum = latinAlf ++ ['0'..'9']

--------------------------------------------------------
-- integerTerm - з числа в вираз
integerTerm :: Int ->  Term
integerTerm n  = (Abs "s" (Abs "z" (buildTerm n))) 
  where buildTerm 0 = Var "z" 
        buildTerm j = (App (Var "s") (buildTerm (j-1)))  

--  New Name -- якщо імя dddname, де ddd-цифри і n-буква, то початкове імя - name 
-- якщо змінна ccc, то її нові імена 0ccc,...,9ccc,09ccc,...
-- цифри на початку - це створення нового імені (problem name capture)
newVar :: [String] -> String -> String
newVar fvs nm = (until (\n -> notElem n fvs) next) (next nm)   -- flip elem fvs
  where next n@(c:_)| c=='9'    = '0':n 
        next (c:cx) | isDigit c = (succ c):cx 
        next n      = '0':n

--------------------------------------------------------
-- Тестові приклади
term0, term0a, term1, term1a, term1b, term1c :: Term
term0 = Abs "s" (Abs "z" (App (Var "s") (App (Var "s") (Var "z")))) 
term0a = Abs "z" (App (Var "s") (App (Var "s") (Var "z")))
term1 = Abs "y" (App (Abs "x" (Abs "y" (App (Var "x") (Var "y")))) (Var "y"))
term1a = App (Abs "x" (Abs "y" (App (Var "x") (Var "y")))) (Var "y")
term1b = Abs "x" (Abs "y" (App (Var "x") (Var "y")))
term1c = Abs "y" (App (Var "x") (Var "y"))

term2, term2a, termAnd, termTest :: Term
term2 = App (App (Abs "f" (Abs "g" (Abs "x" (App (App (Var "f") (Var "x")) (App (Var "g") (Var "x"))))))
                 (Abs "x" (Abs "y" (Var "x")))
            ) 
            (Abs "x" (Abs "y" (Var "x")))
term2a = App (Var "x") (App (Abs "x" (Var "x")) (App (Abs "x" (Var "x")) (Var "z")))
termAnd = Abs "x" (Abs "y" (App (App  (Var "x") (Var "y")) (Var "false")))
termTest = Abs "x" (Abs "x" (Abs "y" (Var "y")))

cont1 :: Contex
cont1 = [("true",Abs "x" (Abs "y" (Var "x")))
        ,("false",Abs "x" (Abs "y" (Var "y")))
        ,("test",Abs "l" (Abs "m" (Abs "n" (App (App (Var "l") (Var "m")) (Var "n")))))
        ,("iszero",Abs "n" (App (App (Var "n") (Abs "x" (Var "false"))) (Var "true")))
        ,("plus",Abs "m" (Abs "n" (Abs "f" (Abs "x" (App (App (Var "n") (Var "f")) (App (App (Var "m") (Var "f")) (Var "x")))))))
        ,("mult",Abs "m" (Abs "n" (Abs "f" (App (Var "m") (App (Var "n") (Var "f"))))))
        ,("pred",Abs "n" (Abs "f" (Abs "x" (App (App (App (Var "n") (Abs "g" (Abs "h" (App (Var "h") (App (Var "g") (Var "f")))))) (Abs "u" (Var "x"))) (Abs "x" (Var "x"))))))
        ,("fixM",Abs "f" (App (Abs "x" (App (Var "f") (Abs "y" (App (App (Var "x") (Var "x")) (Var "y"))))) (Abs "x" (App (Var "f") (Abs "y" (App (App (Var "x") (Var "x")) (Var "y")))))))
        ,("sumR",Abs "r" (Abs "n" (App (App (App (Var "test") (App (Var "iszero") (Var "n"))) (Nmb 0)) (App (App (Var "plus") (Var "n")) (App (Var "r") (App (Var "pred") (Var "n")))))))
        ,("factR",Abs "fact" (Abs "n" (App (App (App (Var "test") (App (Var "iszero") (Var "n"))) (Nmb 1)) (App (App (Var "mult") (Var "n")) (App (Var "fact") (App (Var "pred") (Var "n")))))))
        ,("sum",App (Var "fixM") (Var "sumR"))
        ,("factor",App (Var "fixM") (Var "factR"))
        ]

termS2 :: String 
termS2 = "(\\f g x. f x (g x))   (\\x y .x) (\\x y .x)"