{-# OPTIONS_GHC -Wall #-}

module P07_BTrees where

data BinTreeM a
  = EmptyM
  | NodeM a Int (BinTreeM a) (BinTreeM a)
  deriving (Show, Eq)

-- B-дерево порядка t (NodeB kl tl) =>
--      t-1 <= length kl <= 2*t-1  &&  t <= length tl <= 2*t
data Btree a = NodeB [a] [Btree a] deriving (Show, Eq)

-- головні характеристики B-дерево  (BInform heigth min max)
data BInform a = BInform {hB :: Int, minB :: a, maxB :: a} deriving (Show, Eq)

-- Задача 1 ------------------------------------
isSearch :: (Ord a) => BinTreeM a -> Bool
isSearch EmptyM = True
isSearch (NodeM v k tl tr) =
  k > 0
    && maybe True (< v) (getVal tl)
    && maybe True (> v) (getVal tr)
    && isSearch tl
    && isSearch tr

getVal :: (Ord a) => BinTreeM a -> Maybe a
getVal EmptyM          = Nothing
getVal (NodeM v _ _ _) = Just v

-- Задача 2 ------------------------------------
elemSearch :: (Ord a) => BinTreeM a -> a -> Bool
elemSearch EmptyM _ = False
elemSearch (NodeM v _ tl tr) sv =
  v == sv || elemSearch tl sv || elemSearch tr sv

-- Задача 3 ------------------------------------
insSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a
insSearch EmptyM v = NodeM v 1 EmptyM EmptyM
insSearch (NodeM u k tl tr) v
  | v < u = NodeM u k (insSearch tl v) tr
  | v > u = NodeM u k tl (insSearch tr v)
  | otherwise = NodeM u (k + 1) tl tr

-- Задача 4 ------------------------------------
delSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a
delSearch EmptyM _ = EmptyM
delSearch (NodeM u k tl tr) v
  | v < u = NodeM u k (delSearch tl v) tr
  | v > u = NodeM u k tl (delSearch tr v)
  | k > 1 = NodeM u (k - 1) tl tr
  | tl == EmptyM && tr == EmptyM = EmptyM
  | tl /= EmptyM && tr == EmptyM = tl
  | tl == EmptyM && tr /= EmptyM = tr
  | otherwise =
      let (mx, mk) = maxN tl
          tl1 = delFull tl mx
       in NodeM mx mk tl1 tr

delFull :: (Ord a) => BinTreeM a -> a -> BinTreeM a
delFull EmptyM _ = EmptyM
delFull (NodeM u k tl tr) v
  | v < u = NodeM u k (delFull tl v) tr
  | v > u = NodeM u k tl (delFull tr v)
  | tl == EmptyM && tr == EmptyM = EmptyM
  | tl /= EmptyM && tr == EmptyM = tl
  | tl == EmptyM && tr /= EmptyM = tr
  | otherwise =
      let (mx, mk) = maxN tl
          tl1 = delFull tl mx
       in NodeM mx mk tl1 tr

maxN :: (Ord a) => BinTreeM a -> (a, Int)
maxN EmptyM           = error "empty tree"
maxN (NodeM u k _ tr) = if tr == EmptyM then (u, k) else maxN tr

-- Задача 5 ------------------------------------
sortList :: (Ord a) => [a] -> [a]
sortList xs = lsInOrder $ foldl insSearch EmptyM xs

lsInOrder :: (Ord a) => BinTreeM a -> [a]
lsInOrder EmptyM            = []
lsInOrder (NodeM u k tl tr) = lsInOrder tl ++ replicate k u ++ lsInOrder tr

-- Задача 6 ------------------------------------
findBInform :: (Bounded a, Ord a) => Btree a -> BInform a
findBInform t = BInform (height t) (bMin t) (bMax t)

height :: (Bounded a, Ord a) => Btree a -> Int
height (NodeB _ ts) = if null ts then 0 else 1 + maximum (map height ts)

bMin :: (Bounded a, Ord a) => Btree a -> a
bMin (NodeB vs ts) = if null ts then head vs else (bMin . head) ts

bMax :: (Bounded a, Ord a) => Btree a -> a
bMax (NodeB vs ts) = if null ts then last vs else (bMax . last) ts

-- Задача 7 ------------------------------------
isBtree :: (Bounded a, Ord a) => Int -> Btree a -> Bool
isBtree t tr@(NodeB _ sts) =
  foldl (\res cld -> res && checkNodeN t cld) True sts
    && checkRoot t tr
    && checkNodeKeys tr
    && allEq (getLeafDepths 0 tr)

checkNodeN :: (Bounded a, Ord a) => Int -> Btree a -> Bool
checkNodeN t (NodeB vs sts) =
  let nKeys = length vs
      nCld = length sts
   in t - 1 <= nKeys
        && nKeys <= 2 * t - 1
        && (nCld == 0 || t <= nCld && nCld <= 2 * t)
        && foldl (\res cld -> res && checkNodeN t cld) True sts

checkRoot :: (Bounded a, Ord a) => Int -> Btree a -> Bool
checkRoot t (NodeB vs sts) =
  let nKeys = length vs
      nCld = length sts
   in (nKeys == 0 || 1 <= nKeys && nKeys <= 2 * t - 1)
        && (nCld == 0 || 2 <= nCld && nCld <= 2 * t)

checkNodeKeys :: (Bounded a, Ord a) => Btree a -> Bool
checkNodeKeys (NodeB ks sts) =
  sortList ks == ks
    && ( null sts
           || length sts == length ks + 1
             && checkKeysOrd (NodeB ks sts)
             && foldl (\res cld -> res && checkNodeKeys cld) True sts
       )

checkKeysOrd :: (Bounded a, Ord a) => Btree a -> Bool
checkKeysOrd (NodeB ks sts) =
  let zipped = zip (Nothing : map Just ks) (map Just ks ++ [Nothing])
   in foldl
        ( \res (NodeB cks _, i) ->
            res
              && all
                ( \ck ->
                    let (lo, hi) = zipped !! i
                     in maybe True (<= ck) lo
                          && maybe True (>= ck) hi
                )
                cks
        )
        True
        (zip sts [0 ..])

getLeafDepths :: (Bounded a, Ord a) => Int -> Btree a -> [Int]
getLeafDepths dep (NodeB _ sts) =
  if null sts
    then [dep]
    else concatMap (getLeafDepths (dep + 1)) sts

allEq :: (Eq a) => [a] -> Bool
allEq xs = all (== head xs) (tail xs)

-- Задача 8 ------------------------------------
eqBtree :: (Bounded a, Ord a) => Int -> Btree a -> Btree a -> Bool
eqBtree t tr1 tr2 =
  isBtree t tr1
    && isBtree t tr2
    && btToList tr1 == btToList tr2

btToList :: (Ord a) => Btree a -> [a]
btToList (NodeB ks sts) =
  if null sts
    then ks
    else
      let keys = [[k] | k <- ks] ++ [[]]
          clds = [btToList st | st <- sts]
       in concat (zipWith (++) clds keys)

-- Задача 9 ------------------------------------
elemBtree :: Ord a => Btree a -> a -> Bool
elemBtree tr v = v `elem` btToList tr

-- Задача 10 ------------------------------------
insBtree :: Ord a => Int -> Btree a -> a -> Btree a
insBtree t tr@(NodeB ks sts) v
  | null sts = NodeB (insertKey v ks) []
  | isFull t tr =
      let (bt1, md, bt2) = splitAtB t tr
       in NodeB [md] [insBtree t bt1 v, bt2]
  | otherwise =
      let (kl1, kl2, tl1, bt, tl2) = decomposeNodeB v ks sts
       in if isFull t bt
            then
              let (bt1, md, bt2) = splitAtB t bt
               in NodeB (kl1 ++ [md] ++ kl2) (tl1 ++ [insBtree t bt1 v, bt2] ++ tl2)
            else NodeB ks (tl1 ++ [insBtree t bt v] ++ tl2)

isFull :: Ord a => Int -> Btree a -> Bool
isFull t (NodeB ks _) = length ks == 2 * t - 1

position :: Ord a => a -> [a] -> Int
position v xs =
  let res = [i | (x, i) <- zip xs [0 ..], x >= v]
   in if null res then length xs else head res

insertKey :: Ord a => a -> [a] -> [a]
insertKey v xs =
  let i = position v xs
   in take i xs ++ [v] ++ drop i xs

decomposeNodeB ::
  Ord a =>
  a ->
  [a] ->
  [Btree a] ->
  ([a], [a], [Btree a], Btree a, [Btree a])
decomposeNodeB v kl tl =
  let i = position v kl
   in (take i kl, drop i kl, take i tl, tl !! i, drop (i + 1) tl)

splitAtB :: Ord a => Int -> Btree a -> (Btree a, a, Btree a)
splitAtB t (NodeB kl tl) =
  let md = kl !! (t - 1)
      bt1 = NodeB (take (t - 1) kl) (take t tl)
      bt2 = NodeB (drop t kl) (drop t tl)
   in (bt1, md, bt2)

---------------------Тестові дані - Дерева пошуку -------
bm :: BinTreeM Char
bm =
  NodeM
    't'
    2
    ( NodeM
        'a'
        1
        EmptyM
        ( NodeM
            'e'
            1
            (NodeM 'd' 2 EmptyM EmptyM)
            (NodeM 'f' 1 EmptyM EmptyM)
        )
    )
    (NodeM 'w' 2 EmptyM EmptyM)

tBt1 :: Btree Char
tBt1 =
  NodeB
    "L"
    [ NodeB
        "DG"
        [ NodeB "AC" [],
          NodeB "EE" [],
          NodeB "HK" []
        ],
      NodeB
        "PU"
        [ NodeB "MM" [],
          NodeB "RS" [],
          NodeB "UW" []
        ]
    ]

tBt2 :: Btree Char
tBt2 =
  NodeB
    "GP"
    [ NodeB "ACDEE" [],
      NodeB "HKLMM" [],
      NodeB "RSUUW" []
    ]

tBt5 :: Btree Char
tBt5 =
  NodeB
    "GMPX"
    [ NodeB "ACDE" [],
      NodeB "JK" [],
      NodeB "NO" [],
      NodeB "RSTUV" [],
      NodeB "YZ" []
    ]

tBt6 :: Btree Char
tBt6 =
  NodeB
    "GMPX"
    [ NodeB "ABCDE" [],
      NodeB "JK" [],
      NodeB "NO" [],
      NodeB "RSTUV" [],
      NodeB "YZ" []
    ]

tBt7 :: Btree Char
tBt7 =
  NodeB
    "GMPTX"
    [ NodeB "ABCDE" [],
      NodeB "JK" [],
      NodeB "NO" [],
      NodeB "QRS" [],
      NodeB "UV" [],
      NodeB "YZ" []
    ]

tBt8 :: Btree Char
tBt8 =
  NodeB
    "P"
    [ NodeB
        "GM"
        [ NodeB "ABCDE" [],
          NodeB "JKL" [],
          NodeB "NO" []
        ],
      NodeB
        "TX"
        [ NodeB "QRS" [],
          NodeB "UV" [],
          NodeB "YZ" []
        ]
    ]

tBt9 :: Btree Char
tBt9 =
  NodeB
    "P"
    [ NodeB
        "CGM"
        [ NodeB "AB" [],
          NodeB "DEF" [],
          NodeB "JKL" [],
          NodeB "NO" []
        ],
      NodeB
        "TX"
        [ NodeB "QRS" [],
          NodeB "UV" [],
          NodeB "YZ" []
        ]
    ]
