-- Universidade Federal de Pernambuco
-- Centro de Informática (CIn)
-- Alunos: Victor Sin Yu Chen & Lucas de Souza Albuquerque
-- Login: vsyc & lsa2

---------------------------------------------------------------------------------------------

{-Trabalho 8, Questão 1-}
    {-listPartitioner-}
quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) = (quicksort [min | min <- xs, min < x]) ++ [x] ++ (quicksort [maj | maj <- xs, maj > x])

func :: [Int] -> [Int] -> [[Int]]
func [] l = []
func (x:xs) l = if xs /= [] then [quicksort [min | min <- l, min <= x]] ++ func xs [maj | maj <- l, maj > x]
                else [quicksort [min | min <- l, min <= x]] ++ [quicksort [max | max <- l, max > x]]

listPartitioner :: [Int] -> ([Int] -> [[Int]])
listPartitioner [] = func []
listPartitioner x = func (quicksort x)

-- Exercícios
-- invertF
invertF :: (t -> u -> v) -> (u -> t -> v)
invertF f = \x y -> f y x

-- firstEl
firstEl :: ([(t,t)] -> [t])
firstEl = (\x -> map (fst) x)

-- lengthGreaterThan
lengthGreaterThan :: ([[t]] -> Int -> [[t]])
lengthGreaterThan = \x y -> filter ((>y).length) x

-- joinList
filterUseful :: (Eq t) => [t] -> [t] -> [t]
filterUseful r l | r == [] || l == [] = r
                 | elem (head r) l = filterUseful (tail r) l
                 | otherwise = (head r) : filterUseful (tail r) l

aux :: (Eq t) => [[t]] -> [[t]]
aux x = if x == [] then []
                  else filterUseful (head x) (foldr (++) [] (tail x)) : aux (tail x)

joinList :: (Eq t, Ord t) => ([[t]] -> [t])
joinList = \x -> foldr (++) [] (aux x)

-- mapFold
myFold :: (t -> u -> u) -> u -> [t] -> u
myFold _ b [] = b
myFold f b (x:xs) = f x (myFold f b xs)

mapFold :: (t -> u -> u) -> [u] -> [[t] -> u]
mapFold _ [] = []
mapFold f l = (\x -> myFold f (head l) x) : mapFold f (tail l)

-- teste: map ($[3,4,5]) ((mapFold) (+) [0,1,2])
-- resultado: [12,13,14]

-- sumX
sum :: (Num t) => t -> t -> t
sum x y = x + y

sumX :: (Num t) => t -> ([t] -> [t])
sumX n = map (Main.sum n)

-- maxEl
maxEl :: (Ord t) => ([t] -> t)
maxEl = maximum

-- isomorficas
data BTree t = NilBT
          | Node t (BTree t) (BTree t) deriving (Eq, Show)

isomorficas :: (Eq t) => BTree t -> (BTree t -> Bool)
isomorficas (NilBT) = (== (NilBT))
isomorficas x = (== (x)) -- isso aqui é falho

-- createPair
pair :: (Eq t) => [t] -> [t] -> [(t,t)]
pair x y | x == [] || y == [] = []
         | otherwise = ((head x), (head y)) : pair (tail x) (tail y)

createPair :: (Eq t) => [t] -> ([t] -> [(t,t)])
createPair l = \x -> pair l x