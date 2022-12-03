{-# OPTIONS_GHC -Wall #-}
module PR10_ProceduralLang where

import Data.List

-- розглядаємо лише цілі дані: скаляри  і масиви  
--------------------------------------------------------------------
type Id    = String
data Value = I Int | A [(Int, Int)]  deriving (Eq, Show)
data Op    = Add | Minus | Mul | Less | Equal | Index  deriving (Eq, Show)

data Exp = Const Int 
         | Var Id 
         | OpApp Op Exp Exp 
         | Cond Exp Exp Exp 
         | FunApp Id [Exp] 
         deriving (Eq, Show)

data Stmt = Assign Id Exp 
          | AssignA Id Exp Exp 
          | If Exp Stmt Stmt 
          | While Exp Stmt 
          | Call Id [Exp] 
          | Block [VarDef] [Stmt]
          deriving (Eq, Show)

data VarDef  =  Arr Id | Int Id deriving (Eq, Show)

type FunDef  =  (Id, ([VarDef], Exp))
-- функції повертають лише цілі скалярні дані, не використовують глобальні дані (чисті!!)
type ProcDef = (Id, ([VarDef], Stmt))
type Program = ([VarDef], [FunDef], [ProcDef])

type StateP  = [(Id, Value)]  -- стек даних

data Type    = At | It  deriving (Eq, Show)
type FunEnv  = [(Id,[Type])]
type ProcEnv = [(Id,[Type])]
type VarEnv  = [(Id,Type)]

-- Задача 1 ------------------------------------
updateValue :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
updateValue a b ps =
  case findIndex (==a) (map fst ps) of
    Nothing -> ps ++ [(a,b)]
    (Just i) -> take i ps ++ [(a,b)] ++ drop (i+1) ps
              
-- Задача 2 ------------------------------------
updateArray :: Value -> Value -> Value -> Value
updateArray (A ps) (I i) (I v) = A $ updateValue i v ps
updateArray x _ _ = x 

-- Задача 3 ------------------------------------
applyOp :: Op -> Value -> Value -> Value 
applyOp Add (I v) (I u) = I (v + u)
applyOp Minus (I v) (I u) = I (v - u)
applyOp Mul (I v) (I u) = I (v * u)
applyOp Less (I v) (I u) = toVal $ v < u
applyOp Equal (I v) (I u) = toVal $ v == u
applyOp Index (A ps) (I i) =
  case lookup i ps of
    Nothing -> (I 0)
    (Just v) -> (I v) 
applyOp a b c = error $ "Invalid operand types " ++ show a ++ " (" ++ show b ++ ") (" ++ show c ++ ")"

toVal :: Bool -> Value
toVal v = I (if v then 1 else 0)

valToBool :: Value -> Bool
valToBool (I 0) = False
valToBool _ = True

-- Задача 4 ------------------------------------
evExp :: Exp -> [FunDef] -> StateP -> Value 
evExp (Const x) _ _ = (I x)

evExp (Var s) _ st = case lookup s st of
  (Just ex) -> ex
  Nothing -> error $ "unknown identifier " ++ s

evExp (OpApp op ex1 ex2) dfx st = applyOp op (evExp ex1 dfx st) (evExp ex2 dfx st) 

evExp (Cond cnd ex1 ex2) dfx st = 
  let ex = if valToBool (evExp cnd dfx st) then ex1 else ex2
  in evExp ex dfx st

evExp (FunApp nm exs) dfx st = case lookup nm dfx of
  Nothing -> error $ "unknown function " ++ nm
  (Just (as, ef)) ->
    let vs = evArgs exs dfx st
        new = zip (map getVarName as) vs
    in evExp ef dfx new

evArgs :: [Exp] -> [FunDef] -> StateP -> [Value]  
evArgs exs dfx st = map (\ex -> evExp ex dfx st) exs

getVarName :: VarDef -> Id
getVarName (Arr i) = i
getVarName (Int i) = i

-- Задача 5 ------------------------------------
evStmt :: Stmt -> [FunDef] -> [ProcDef] -> StateP -> StateP
evStmt (Assign nm ex) dfx _ st = updateValue nm (evExp ex dfx st) st

evStmt (AssignA nm ix ex) dfx _ st = case lookup nm st of
  Nothing -> error $ "unknown identifier " ++ nm
  (Just arr) -> updateValue nm newArr st
    where newArr = updateArray arr (evExp ix dfx st) (evExp ex dfx st)

evStmt (If cnd stmt1 stmt2) dfx dpx st =
  let stmt = if valToBool (evExp cnd dfx st) then stmt1 else stmt2
  in evStmt stmt dfx dpx st

evStmt (While cnd stmt) dfx dpx st =
  until (not . valToBool . (evExp cnd dfx)) (evStmt stmt dfx dpx) st

evStmt (Call nm args) dfx dpx st = case lookup nm dpx of
  Nothing -> error $ "unknown function " ++ nm
  (Just (as, stmt)) ->
    let vs = evArgs args dfx st
        varNms =  map getVarName as
        newSt = zip varNms vs
        resSt = evStmt stmt dfx dpx newSt
    in [var | var@(vnm,_) <- resSt, notElem vnm varNms]

evStmt (Block vars stmts) dfx dpx st =
  let varNms = map getVarName vars
      newSt = st ++ [initv var | var <- vars]
      resSt = foldl (\curSt stmt -> evStmt stmt dfx dpx curSt) newSt stmts
  in [var | var@(nm,_) <- resSt, notElem nm varNms]

-- Задача 6 ------------------------------------
iswfExp :: Exp -> VarEnv -> FunEnv -> Maybe Type   
iswfExp (Const _) _ _ = Just It
iswfExp (Var nm) ve _ = lookup nm ve

iswfExp (OpApp op ex1 ex2) ve fe = do
  t1 <- iswfExp ex1 ve fe
  t2 <- iswfExp ex2 ve fe
  iswfOp op [t1, t2]

iswfExp (Cond cnd ex1 ex2) ve fe = do
  tcnd <- iswfExp cnd ve fe
  t1 <- iswfExp ex1 ve fe
  t2 <- iswfExp ex2 ve fe
  iswfCond [tcnd, t1, t2]

iswfExp (FunApp nm args) ve fe = do
   params <- lookup nm fe
   let argTs = map (\x -> iswfExp x ve fe) args
       zipArgs = zipWith (\a b -> maybe False (==b) a) argTs params
   if (length argTs == length params && all (==True) zipArgs)
    then Just It
    else Nothing

-- Задача 7 ------------------------------------
iswfStmt :: Stmt -> VarEnv -> FunEnv -> ProcEnv -> Bool
iswfStmt (Assign nm ex) ve fe _ = maybe False id (do
  tv <- lookup nm ve
  te <- iswfExp ex ve fe
  return (iswfAssign [tv, te]))

iswfStmt (AssignA nm i ex) ve fe _ = maybe False id (do
  tv <- lookup nm ve
  ti <- iswfExp i ve fe
  te <- iswfExp ex ve fe
  return (iswfAssignA [tv, ti, te]))

iswfStmt (If cnd stmt1 stmt2) ve fe pe =
  maybe False (==It) (iswfExp cnd ve fe)
  && iswfStmt stmt1 ve fe pe
  && iswfStmt stmt2 ve fe pe

iswfStmt (While cnd stmt) ve fe pe =
  maybe False (==It) (iswfExp cnd ve fe)
  && iswfStmt stmt ve fe pe

iswfStmt (Call nm args) ve fe pe = maybe False id (do
   params <- lookup nm pe
   let argTs = map (\x -> iswfExp x ve fe) args
       zipArgs = zipWith (\a b -> maybe False (==b) a) argTs params
   return (length argTs == length params && all (==True) zipArgs))

iswfStmt (Block vars stmts) ve fe pe =
  let varNms = map getVarName vars
      addVe = ve ++ (map varDefToEnv vars)
  in uniq varNms
     && all (==True) [ iswfStmt stm addVe fe pe | stm <- stmts]

varDefToEnv :: VarDef -> (Id,Type)
varDefToEnv (Arr nm) = (nm, At)
varDefToEnv (Int nm) = (nm, It)

iswfAssign :: [Type] -> Bool
iswfAssign [It,It] = True 
iswfAssign _       = False  

-- Задача 8 ------------------------------------
iswfFunDef :: FunDef -> FunEnv -> Bool
iswfFunDef (_, (vars, body)) fe =
  uniq (map fst varsEnv)
  && maybe False (==It) (iswfExp body varsEnv fe)
  where varsEnv = map varDefToEnv vars


iswfProcDef :: ProcDef -> VarEnv -> FunEnv -> ProcEnv -> Bool
iswfProcDef (_, (args, body)) ve fe pe =
  uniq (map fst argEnv)
  && iswfStmt body (ve ++ argEnv) fe pe
  where argEnv = map varDefToEnv args

-- Задача 9 ------------------------------------
iswfProgram :: Program -> Bool
iswfProgram (dvs, dfx, dpx) =
  let varsEnv = map varDefToEnv dvs
      fnEnv = map fnDfToEnv dfx
      procsEnv = map procDfToEnv dpx
  in uniq (map fst varsEnv)
     && uniq (map fst fnEnv)
     && all (`iswfFunDef` fnEnv) dfx
     && uniq (map fst procsEnv)
     && all (\x -> iswfProcDef x varsEnv fnEnv procsEnv) dpx
     && maybe False (null . fst) (lookup "main" dpx)


fnDfToEnv :: FunDef -> (Id, [Type])
fnDfToEnv (nm, (dvs, _)) =
  (nm, map (snd . varDefToEnv) dvs)

procDfToEnv :: ProcDef -> (Id, [Type])
procDfToEnv (nm, (dvs, _)) =
  (nm, map (snd . varDefToEnv) dvs)

--- Допоміжні функції -----------------------------
uniq :: (Eq a) => [a] -> Bool
uniq xs = nub xs == xs

lookUp :: Eq a => a -> [(a,b)] -> b
-- Передумова: Пара з ключом a є в списку пар abx
lookUp a abx = maybe (error "lookUp") id (lookup a abx) 

-- формує початкове значення змінної
initv :: VarDef -> (Id, Value)
initv (Arr v) = (v, A [])
initv (Int v) = (v, I 0) 

-- Реалізація виконання програми 
evProgram :: Program -> StateP 
evProgram (dvx, dfx, dpx) = 
   let sb = map initv dvx 
       ( _, s) = lookUp "main" dpx      
   in evStmt s dfx dpx sb   

--  iswfOp o ts - перевіряє коректність типів операндів ts 
--     бінарної операції o і формує тип результату Just t або Nothing  
iswfOp :: Op -> [Type] -> Maybe Type 
iswfOp Add   [It,It] = Just It
iswfOp Minus [It,It] = Just It
iswfOp Mul   [It,It] = Just It
iswfOp Less  [It,It] = Just It
iswfOp Equal [It,It] = Just It
iswfOp Index [At,It] = Just It
iswfOp _      _      = Nothing

--  iswfCond ts - перевіряє коректність  типів операндів ts
--     умовного виразу і формує тип результату Just t або Nothing 
iswfCond :: [Type] -> Maybe Type 
iswfCond [It,It,It] = Just It
iswfCond [It,At,At] = Just At
iswfCond _          = Nothing 

-- iswfAssignA ts перевіряє коректність  типів операндів ts
--   операції присвоювання значення елементу масива 
iswfAssignA :: [Type] -> Bool
iswfAssignA [At,It,It] = True 
iswfAssignA _          = False  

---- Дані для тестування  -----------------------
-- Стан для тестування
sampleState :: StateP
sampleState = [("x",I 5),("y",I 2),("a", A [(2,3),(0,4), (1,2)])]

varEnv :: VarEnv 
varEnv = [("x",It), ("y",It), ("a",At)]

-- Функція максимум двох чисел 
-- func biggest(m,n)= (m<n ? n : m)
biggest :: FunDef
biggest =("biggest",
          ([Int "m", Int "n"], 
           Cond (OpApp  Less (Var "m") (Var "n"))  (Var "n")  (Var "m")                                                                
           )
         )
-- Функція, що обчислює число Фібоначчі
-- func fib(n) = (n<3 ? 1 : fib(n-1) + fib(n-2))
fib :: FunDef
fib = ("fib",
       ([Int "n"], 
        Cond (OpApp Less (Var "n") (Const 3))
             (Const 1)
             (OpApp Add (FunApp "fib" [OpApp Minus (Var "n") (Const 1)])
                        (FunApp "fib" [OpApp Minus (Var "n") (Const 2)]))
       )
      )

-- Функція - сума елементів масиву 0..n ...
-- func sumA(a[],n) = (n<0 ? 0 : a[n] + sumA (a,n-1))
sumA :: FunDef
sumA = ("sumA",
        ([Arr "a", Int "n"],
         Cond (OpApp Less (Var "n") (Const 0)) 
              (Const 0)
              (OpApp Add (OpApp Index (Var "a") (Var "n"))
                         (FunApp "sumA" [Var "a", OpApp Minus (Var "n")(Const 1)])
              )
        )
       )

funEnv :: FunEnv
funEnv = [("biggest",[It,It]),("fib", [It]),("sumA",[At,It])]

-- Приклад оператору - блоку 
sampleBlock :: Stmt 
sampleBlock = Block [Arr "b"]
  [
    AssignA "b" (Const 0) (Const 9)
    ,AssignA "b" (Const 2) (Const 5)
    ,AssignA "b" (Const 3) (Const 7)
    ,AssignA "b" (Const 5) (Const 1)
    ,Call "sumA1" [Var "b", Const 5]
  ]

sampleBlock1 :: Stmt 
sampleBlock1 = 
    Block [Arr "b"]
    [
       AssignA "b" (Const 0) (Const 9)
    ]


-- Процедура - додавання двох чисел...
-- proc gAdd(x,y) gSum = x + y 
gAdd :: ProcDef
gAdd = ("gAdd", 
        ([Int "x", Int "y"], 
         Assign "gSum" (OpApp Add (Var "x") (Var "y"))
        )
       )

-- Процедура - сума елементів масиву 0..n ...
-- proc sumA1(a[],n) {i;limit;
--      sA=0; i=0; limit=n+1;
--      while (i<limit){sA=sA+a[i]; i=i+1}
--                   }
sumA1 :: ProcDef
sumA1 = ("sumA1",
         ([Arr "a", Int "n"], 
          Block [Int "i", Int "limit"] 
            [Assign "sA" (Const 0), Assign "i" (Const 0),
             Assign "limit" (OpApp Add (Var "n") (Const 1)),
             While (OpApp Less (Var "i") (Var "limit"))
                   (Block [] 
                     [Assign "sA" (OpApp Add (Var "sA")
                                  (OpApp Index (Var "a") (Var "i"))),
                      Assign "i" (OpApp Add (Var "i") (Const 1))
                     ]
                   )
            ]
         )
        )

procEnv :: ProcEnv 
procEnv = [("gAdd",[It,It]),("sumA1",[At,It])]

-- Повні програми
-- gSum; 
-- proc gAdd(x,y) gSum = x + y 
-- proc main() call gAdd(5,10)   
pr1 :: Program
pr1 = ([Int "gSum"], [], [gAdd, ("main",([],Call "gAdd" [Const  5, Const 10]))])

-- sA
-- proc sumA1(a[],n) {i;limit; .... } 
-- proc main() {b[]; b[0]=9; b[2]=5; b[3]=7; b[5]=1;
--                   call sumA1 (b,5)
--             }

pr2 :: Program
pr2 = ([Int "sA"], [], 
       [sumA1, 
        ("main",([], sampleBlock))
       ])
