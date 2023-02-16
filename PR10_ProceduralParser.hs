module PR10_ProceduralParser where

import           PR10_ProceduralLang
import           Text.ParserCombinators.Parsec

-- UTILS
number :: Parser Int
number = do
  s <- option "" (string "-")
  ds <- many1 digit
  return $ read (s ++ ds)

infOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infOp x f = string x >> return f

lexem :: Parser a -> Parser a
lexem p = do a <- p; spaces; return a

reserved :: String -> Parser ()
reserved s = do _ <- string s; spaces

parens :: Parser a -> Parser a
parens p = do reserved "("; n <- lexem p; reserved ")"; return n

int :: Parser Exp
int = do n <- lexem number; return (Const n)

latinAlf :: String
latinAlf = ['A' .. 'Z'] ++ ['a' .. 'z']

nums :: String
nums = ['0' .. '9']

firstLetter :: String
firstLetter = latinAlf

subseqLetter :: String
subseqLetter = latinAlf ++ nums ++ "_$#@"

idf :: Parser String
idf = do
  st <- oneOf firstLetter
  nx <- many (oneOf subseqLetter)
  return (st : nx)

idp :: Parser Exp
idp = do
  st <- lexem idf
  return (Var st)

-- ARITHMETIC
addopA, mulopA :: Parser (Exp -> Exp -> Exp)
addopA = infOp "+" (OpApp Add) <|> infOp "-" (OpApp Minus)
mulopA = infOp "*" (OpApp Mul)

cmpOpA =
  infOp "==" (OpApp Equal)
    <|> infOp "<" (OpApp Less)

-- EXPRESSIONS
exprA, termA, factorA :: Parser Exp
factorA = int <|> try funcApp <|> try indexExp <|> idp <|> parens exprA
termA = chainl1 factorA (lexem mulopA)

eqTermA = chainl1 termA (lexem addopA)

exprA = condExp (chainl1 eqTermA (lexem cmpOpA))

condExp :: Parser Exp -> Parser Exp
condExp p = do
  expr <- p
  cond expr <|> return expr
  where
    cond expr = do
      reserved "?"
      thenV <- condExp p
      reserved ":"
      elseV <- condExp p
      return (Cond expr thenV elseV)

funcApp :: Parser Exp
funcApp = do
  nm <- lexem idf
  FunApp nm <$> callArgs

indexOp :: Parser Exp
indexOp = do
  reserved "["
  ix <- exprA
  reserved "]"
  return ix

indexExp :: Parser Exp
indexExp = do
  nm <- lexem idf
  OpApp Index (Var nm) <$> indexOp

callArgs :: Parser [Exp]
callArgs = parens (option [] (commaSep exprA))

varDef :: Parser VarDef
varDef = do
  nm <- idf
  mbA <- optionMaybe (string "[]")
  return
    ( case mbA of
        Nothing  -> Int nm
        (Just _) -> Arr nm
    )

-- SEQUENCE
data SeqEx a = Lit a | Seq (SeqEx a) (SeqEx a)

seqToList :: SeqEx a -> [a]
seqToList (Lit x)   = [x]
seqToList (Seq x y) = seqToList x ++ seqToList y

commaSep :: Parser a -> Parser [a]
commaSep p = do
  seq <- chainl1 (Lit <$> lexem p) (lexem (infOp "," Seq))
  return (seqToList seq)

commaSepOpt :: Parser a -> Parser [a]
commaSepOpt p = option [] (commaSep p)

-- STATEMENTS
semicolon :: Parser a -> Parser a
semicolon p = do v <- lexem p; reserved ";"; return v

statement :: Parser Stmt
statement = try whileSt <|> try callSt <|> assignSt <|> blockSt

assignSt :: Parser Stmt
assignSt = do
  nm <- lexem idf
  mbIx <- optionMaybe indexOp
  reserved "="
  ex <- exprA
  return
    ( case mbIx of
        Nothing   -> Assign nm ex
        (Just ix) -> AssignA nm ix ex
    )

whileSt :: Parser Stmt
whileSt = do
  reserved "while"
  cnd <- parens exprA
  While cnd <$> statement

callSt :: Parser Stmt
callSt = do
  reserved "call"
  nm <- lexem idf
  Call nm <$> callArgs

blockSt :: Parser Stmt
blockSt = do
  reserved "{"
  vars <- many (try (semicolon varDef))
  stmts <- sepBy statement (reserved ";")
  reserved "}"
  return (Block vars stmts)

-- FUNCTION DEFINITION
funcDef :: Parser FunDef
funcDef = do
  reserved "func"
  nm <- lexem idf
  vars <- parens (commaSep varDef)
  reserved "="
  ex <- exprA
  return (nm, (vars, ex))

-- PROCEDURE DEFINITION
procDef :: Parser ProcDef
procDef = do
  reserved "proc"
  nm <- lexem idf
  vars <- parens (commaSepOpt varDef)
  sm <- statement
  return (nm, (vars, sm))

full :: Parser a -> Parser a
full p = do _ <- spaces; v <- p; eof; return v

-- TESTS
dfSumA :: String
dfSumA = "func sumA(a[],n) = (n<0 ? 0 : a[n] + sumA (a,n-1))"

dfFib :: String
dfFib = "func fib(n) = (n<3 ? 1 : fib(n-1) + fib(n-2))"

dpGAdd :: String
dpGAdd = "proc gAdd(x,y) gSum = x + y "

dpSumA1 :: String
dpSumA1 =
  "proc sumA1(a[],n) {i;limit;"
    ++ "sA=0; i=0; limit=n+1;"
    ++ "while (i<limit){sA=sA+a[i]; i=i+1}"
    ++ "}"

dPr1 :: String
dPr1 =
  "gSum; "
    ++ "proc gAdd(x,y) gSum = x + y "
    ++ "proc main() call gAdd(5,10)   "

dPr2 :: String
dPr2 =
  "sA;"
    ++ "proc sumA1(a[],n) {i;limit;"
    ++ "sA=0; i=0; limit=n+1;"
    ++ "while (i<limit){sA=sA+a[i]; i=i+1}"
    ++ "}"
    ++ "proc main() {b[]; b[0]=9; b[2]=5; b[3]=7; b[5]=1;"
    ++ "call sumA1 (b,5)"
    ++ "}"

prog :: Parser Program
prog = do
  vars <- many (try (semicolon varDef))
  fns <- many funcDef
  procs <- many procDef
  return (vars, fns, procs)

parseOrThrow :: Parser a -> String -> a
parseOrThrow p str =
  case parse (full p) "" str of
    (Left x)  -> error (show x)
    (Right x) -> x

-- parseOrThrow funcDef dfSumA == sumA
-- parseOrThrow funcDef dfFib == fib
-- parseOrThrow procDef dpGAdd == gAdd
-- parseOrThrow procDef dpSumA1 == sumA1
-- parseOrThrow prog dPr1 == pr1
-- parseOrThrow prog dPr2 == pr2
