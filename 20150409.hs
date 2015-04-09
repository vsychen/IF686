-- Universidade Federal de Pernambuco
-- Centro de Informática (CIn)
-- Alunos: Victor Sin Yu Chen & Lucas de Souza Albuquerque
-- Login: vsyc & lsa2

---------------------------------------------------------------------------------------------

{-Trabalho 6, Questão 1-}
    {-Eq and Show Graph-}
data Graph t = NilG
               | Node t [(t, Int)] (Graph t)

checkTerm :: (Eq t) => [(t, Int)] -> (t, Int) -> Bool
checkTerm list x | list == [] = False
                 | (head list) == x = True
                 | otherwise = checkTerm (tail list) x

contains :: (Eq t) => [(t, Int)] -> [(t, Int)] -> Bool
contains [] [] = True
contains [] y = False
contains x [] = False
contains (x:xs) y | xs == [] = (checkTerm y x)
               | otherwise = (checkTerm y x) && (contains xs y)

check :: (Eq t) => [(t, Int)] -> [(t, Int)] -> Bool
check x y = contains x y && contains y x

compareGraph :: (Eq t) => Graph t -> Graph t -> Bool
compareGraph (NilG) (NilG) = True
compareGraph (NilG) (Node _ _ _) = False
compareGraph (Node _ _ _) (NilG) = False
compareGraph (Node id1 list1 graph1) (Node id2 list2 graph2) = (id1 == id2) && (check list1 list2) && (compareGraph graph1 graph2)

showList :: (Show t) => [(t, Int)] -> String
showList [] = ""
showList ((n, w):xs) = (" - " ++ (show n) ++ " " ++ (show w)) ++ Main.showList xs

showGraph :: (Show t) => Graph t -> String
showGraph (NilG) = ""
showGraph (Node id list graph) = ((show id) ++ (Main.showList list)) ++ ". " ++ showGraph graph

{-Trabalho 6, Questão 2-}
	{-DFS in Graph-}
type UnweightedGraph = [(Int, [Int])]

dfs :: UnweightedGraph -> Int -> Bool
dfs [] k = False
dfs g k | fst(head(g)) == k = True
        | otherwise = loopdfs g (makeBool(g)) 1 k (length(snd(head(g))))

loopdfs :: UnweightedGraph -> [Bool] -> Int -> Int -> Int -> Bool
loopdfs [] v a k i = False
loopdfs g v a k 0 = False
loopdfs g v a k n | fst(g!!(a-1)) == k = True
                  | (v!!((snd(g!!(a-1))!!(n-1))-1)) == False = loopdfs g (turnBool v ((snd(g!!(a-1))!!(n-1))-1)) ((snd(g!!(a-1)))!!(n-1)) k (length(snd(g!!(((snd(g!!(a-1)))!!(n-1)-1))))) || loopdfs g v a k (n-1)
				  | otherwise = loopdfs g v a k (n-1)
				  
makeBool :: UnweightedGraph -> [Bool]
makeBool [] = []
makeBool g = False : makeBool (tail(g))

turnBool :: [Bool] -> Int -> [Bool]
turnBool [] n = []
turnBool b n = (take n b) ++ [True] ++ (drop (n+1) b)

-- Exercícios
-- mapSqrTwo
sqrtTwo :: Float -> Float
sqrtTwo x = x ** (0.5)

mapSqrtTwo :: (Float -> Float) -> [Float] -> [Float]
mapSqrtTwo f x | x == [] = []
               | otherwise = (f (head x)) : mapSqrtTwo f (tail x)

-- posicaoAlfabeto
baseAlphabet :: [(Char, Int)]
baseAlphabet = [('a',1),('b',2),('c',3),('d',4),('e',5),('f',6),('g',7),('h',8),('i',9),('j',10),('k',11),('l',12),('m',13),('n',14),('o',15),('p',16),('q',17),('r',18),('s',19),('t',20),('u',21),('v',22),('w',23),('x',24),('y',25),('z',26)]

getPos :: [(Char, Int)] -> Char -> Int
getPos abc x | x == (fst (head abc)) = snd (head abc)
             | otherwise = getPos (tail abc) x

posicaoAlfabeto :: ([(Char, Int)] -> Char -> Int) -> [Char] -> [Int]
posicaoAlfabeto f x | x == [] = []
                    | otherwise = (f baseAlphabet (head x)) : (posicaoAlfabeto f (tail x))

-- mapCL
mapCL :: (t -> t) -> [t] -> [t]
mapCL f x = [(f new) | new <- x]

-- member
member :: (Eq t) => [t] -> t -> Bool
member l e = foldr (||) False (map (== e) l)

-- union
exists :: (Eq t) => [t] -> t -> Bool
exists l e | l == [] = False
           | (head l) == e = True
           | otherwise = exists (tail l) e

kickEq :: (Eq t) => [t] -> [t] -> [t]
kickEq l1 l2 | l1 == [] || l2 == [] = []
             | exists l1 (head l2) = kickEq l1 (tail l2)
             | otherwise = (head l2) : kickEq l1 (tail l2)

group :: [t] -> [t] -> [[t]]
group l1 l2 = l1 : [l2]

union :: (Eq t) => [t] -> [t] -> [t]
union l1 l2 = foldr (++) [] (group l1 (kickEq l1 l2))

-- countString
-- Utiliza getPos e baseAlphabet do grupo de funcoes de posicaoAlfabeto
charToInt :: Char -> Int
charToInt x = getPos baseAlphabet x

stringToInt :: String -> Int
stringToInt s = foldr (+) 0 (map (charToInt) s)

countString :: [String] -> [Int]
countString l = [stringToInt x | x <- l]

{-
-- insertNode
insertNode :: (Eq t) => Tree t -> t -> Tree t
-}
