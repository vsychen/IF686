-- Universidade Federal de Pernambuco
-- Centro de Informática (CIn)
-- Alunos: Victor Sin Yu Chen & Lucas de Souza Albuquerque
-- Login: vsyc & lsa2

---------------------------------------------------------------------------------------------

{-Trabalho 9, Questão 1-}
-- Slide da Aula 02
-- joinLines
type Word = String
type Line = [Word]

joinWords :: [Word] -> String
joinWords x | x == [] = []
            | otherwise = (head x) ++ (joinWords (tail x))

joinLines :: [Line] -> String
joinLines x | x == [] = []
            | otherwise = (joinWords (head x)) ++ (joinLines (tail x))

-- Slide da Aula 05
-- inter
inter :: (Eq t) => [t] -> [t] -> [t]
inter l1 l2 = filter (\x -> elem x l2) l1

-- diff
diff :: (Eq t) => [t] -> [t] -> [t]
diff l1 l2 = filter (\x -> not (elem x l2)) l1

-- mapFilter
mapFilter :: (t -> Bool) -> [[t]] -> [[t]]
mapFilter _ [] = []
mapFilter f (x:xs) = [el | el <- x, f el == True] : mapFilter f xs

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

-- Slide da Aula 06
-- isomorficas
data BTree t = NilBT
          | Node t (BTree t) (BTree t) deriving (Eq, Show)

getEsq :: (Eq t) => BTree t -> BTree t
getEsq (Node _ e _) = e

getDir :: (Eq t) => BTree t -> BTree t
getDir (Node _ _ d) = d

isomorficas :: (Eq t) => BTree t -> (BTree t -> Bool)
isomorficas (NilBT) = (== (NilBT))
isomorficas (Node _ e d) = (\x -> (x /= (NilBT)) && ((normal x) || (inverted x)))
                           where normal x = (((isomorficas e) (getEsq x)) && ((isomorficas d) (getDir x)))
                                 inverted x = (((isomorficas e) (getDir x)) && ((isomorficas d) (getEsq x)))

-- createPair
pair :: (Eq t) => [t] -> [t] -> [(t,t)]
pair x y | x == [] || y == [] = []
         | otherwise = ((head x), (head y)) : pair (tail x) (tail y)

createPair :: (Eq t) => [t] -> ([t] -> [(t,t)])
createPair l = \x -> pair l x

{-Trabalho 9, Questão 2-}
	{-geraFuncaoMenorCaminho-}
type Node t = t
type Edge t = (Node t, Node t, Int)
data Graph t = NilG
               | Graph [Node t] [Edge t] deriving (Eq, Show)

ordEdge :: [Edge Int] -> [Edge Int]
ordEdge [] = []
ordEdge ((x1,x2,d):xs) = [((x1 - 1),(x2 - 1),d),((x2 - 1),(x1 - 1),d)] ++ (ordEdge xs)

newRow :: Int -> Int -> [Int]
newRow l c = if l == 0 then []
             else if l == c then 0 : (newRow (l - 1) c)
             else (99) : (newRow (l - 1) c)

newMatrix :: Int -> Int -> [[Int]]
newMatrix l c = if c == 0 then []
                else (newRow l c) : (newMatrix l (c - 1))

calcPos :: [[Int]] -> Edge Int -> [[Int]]
calcPos m (i, e, d) = (take i m) ++ [((take e (m!!i)) ++ [d] ++ (drop (e + 1) (m!!i)))] ++ (drop (i + 1) m)

origDist :: [[Int]] -> [Edge Int] -> [[Int]]
origDist m [] = m
origDist m e = origDist (calcPos m (head e)) (tail e)

createMatrix :: Graph Int -> [[Int]]
createMatrix (NilG) = []
createMatrix (Graph n e) = origDist (newMatrix (length n) (length n)) (ordEdge e)

minDist :: [[Int]] -> Int -> Int -> Int -> Int
minDist m i j 0 = (m!!(i - 1))!!(j - 1)
minDist m i j k = min (minDist m i j (k - 1)) ((minDist m i k (k - 1)) + (minDist m k j (k - 1)))

geraFuncaoMenorCaminho :: Graph Int -> Int -> Int -> String
geraFuncaoMenorCaminho (NilG) _ _ = ""
geraFuncaoMenorCaminho g i e = (show i) ++ " " ++ (show e) ++ " - " ++ (show (minDist m i e (length m)))
                               where m = createMatrix g

-- geraFuncaoMenorCaminho (Graph [1,2,3,4] [(1,2,5),(2,4,4),(1,3,1),(3,4,1)]) 1 4

-- Exercícios
{-
Determine, sem usar o GHCi, os tipos das seguintes expressões:
map :: (a -> b) -> [a] -> [b]
filter :: (a -> Bool) -> [a] -> [a]
foldr :: (a -> b -> b) -> b -> [a] -> b
(:) :: a -> [a] -> [a]
(.) :: (b -> c) -> (a -> b) -> (a -> c)
(+) :: Num a => a -> a -> a
(++) :: [a] -> [a] -> [a]
$ :: (a -> b) -> a -> b
(!!) :: [a] -> Int -> a

1) foldr (:)
- foldr :: (a -> b -> b) -> b -> [a] -> b
- (:) :: c -> [c] -> [c]

- a = c
- b = [c]
- foldr (:) :: b -> [a] -> b

foldr (:) :: [c] -> [c] -> [c]

2) map.(.)
- map :: (a -> b) -> [a] -> [b]
- (.) :: (d -> e) -> (c -> d) -> (c -> e)
- . :: (g -> h) -> (f -> g) -> (f -> h)

- f = (d -> e)
- g = (a -> b)
- g = ((c -> d) -> (c -> e))
- h = ([a] -> [b])

- a = (c -> d)
- b = (c -> e)

map.(.) :: (f -> h)
map.(.) :: ((d -> e) -> ([a] -> [b]))
map.(.) :: ((d -> e) -> ([c -> d] -> [c -> e]))

3) foldr (+).(.).map ----------------------------------------CHECAR-------------------------------------------------------------
- foldr :: (a -> b -> b) -> b -> [a] -> b
- (+) :: Num c => c -> c -> c
- . :: (e -> f) -> (d -> e) -> (d -> f)
- (.) :: (h -> i) -> (g -> h) -> (g -> i)
- . :: (k -> l) -> (j -> k) -> (j -> l)
- map :: (m -> n) -> [m] -> [n]

(.).map
- j = (m -> n)
- k = (h -> i)
- k = ([m] -> [n])
- l = (g -> h) -> (g -> i)

- h = [m]
- i = [n]

((.).map) :: (j -> l)
((.).map) :: (m -> n) -> ((g -> h) -> (g -> i))
((.).map) :: (m -> n) -> ((g -> [m]) -> (g -> [n]))

(+).((.).map)
- d = (m -> n)
- e = ((g -> [m]) -> (g -> [n]))
- e = (Num c) => c
- f = (Num c) => c -> c

e = ((g -> [m]) -> (g -> [n]))
e = (Num c) => c
c = ((g -> [m]) -> (g -> [n])), mas c pertence a Num

4) map.map.foldr
- map :: (a -> b) -> [a] -> [b]
- . :: (d -> e) -> (c -> d) -> (c -> e)
- map :: (f -> g) -> [f] -> [g]
- . :: (i -> j) -> (h -> i) -> (h -> j)
- foldr :: (k -> l -> l) -> l -> [k] -> l

map.foldr
- h = (k -> l -> l)
- i = (l -> [k] -> l)
- i = (f -> g)
- j = ([f] -> [g])

- f = l
- g = ([k] -> l)

map.foldr :: (h -> j)
map.foldr :: (k -> l -> l) -> ([f] -> [g])
map.foldr :: (k -> l -> l) -> ([l] -> [[k] -> l])

map.(map.foldr)
- c = (k -> l -> l)
- d = ([l] -> [[k] -> l])
- d = (a -> b)
- e = ([a] -> [b])

- a = [l]
- b = [[k] -> l]

map.(map.foldr) :: (c -> e)
map.(map.foldr) :: ((k -> l -> l) -> ([a] -> [b]))
map.(map.foldr) :: ((k -> l -> l) -> ([[l]] -> [[[k] -> l]]))

5) map.((.) (foldr (++) (foldr (++) [] [[1], [2]])))
[1] :: Num t -> [t]
[2] :: Num t -> [t]
(foldr (++) [] [[1], [2]]) :: (Num t) => [t]

- foldr :: (a -> b -> b) -> b -> [a] -> b
- b = [t]
- a = [t]

(foldr (++) (foldr (++) [] [[1], [2]])) :: [a] -> b
(foldr (++) (foldr (++) [] [[1], [2]])) :: (Num t) => [[t]] -> [t]

(.) (foldr (++) (foldr (++) [] [[1], [2]]))
(.) :: (b0 -> c0) -> (a0 -> b0) -> (a0 -> c0)

- a0 = t
- b0 = [[t]]
- c0 = [t]

(.) (foldr (++) (foldr (++) [] [[1], [2]])) :: (Num t) => (a0 -> b0) -> (a0 -> c0)
(.) (foldr (++) (foldr (++) [] [[1], [2]])) :: (Num t) => (t -> [[t]]) -> (t -> [t])

map.((.) (foldr (++) (foldr (++) [] [[1], [2]])))
- map :: (c -> d) -> [c] -> [d]
- . :: (f -> g) -> (e -> f) -> (e -> g)

- e = (t -> [[t]])
- f = (t -> [t])
- f = (c -> d)
- g = ([c] -> [d])

- c = t
- d = [t]

map.((.) (foldr (++) (foldr (++) [] [[1], [2]]))) :: (Num t) => (t -> [[t]]) -> ([c] -> [d])
map.((.) (foldr (++) (foldr (++) [] [[1], [2]]))) :: (Num t) => (t -> [[t]]) -> ([t] -> [[t]])

6) (foldr).(.)$(!!) ----------------------------------------CHECAR-------------------------------------------------------------
- foldr :: (a -> b -> b) -> b -> [a] -> b
- . :: (d -> e) -> (c -> d) -> (c -> e)
- (.) :: (g -> h) -> (f -> g) -> (f -> h)
- $ :: (i -> j) -> i -> j
- (!!) :: [k] -> Int -> k

(.)$(!!)
- i = (g -> h)
- j = (f -> g) -> (f -> h)
- i = [k] -> Int -> k

- g = [k]
- h = (Int -> k)

(.)$(!!) :: j
(.)$(!!) :: (f -> [k]) -> (f -> (Int -> k))

(foldr).(.)$(!!)
- c = (f -> [k])
- d = (f -> (Int -> k))
- d = (a -> b -> b)
- e = (b -> [a] -> b)

- a = f
- (b -> b) = (Int -> k)

- b = Int
- b = k

(foldr).(.)$(!!) :: (c -> e)
(foldr).(.)$(!!) :: ((f -> [k]) -> (b -> [a] -> b))
(foldr).(.)$(!!) :: ((f -> [Int]) -> (Int -> [f] -> Int))
-}