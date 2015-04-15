-- Universidade Federal de Pernambuco
-- Centro de Informática (CIn)
-- Alunos: Victor Sin Yu Chen & Lucas de Souza Albuquerque
-- Login: vsyc & lsa2

---------------------------------------------------------------------------------------------

{-Trabalho 6, Questão 1-}
    {-Eq and Show Graph-}
type Node t = t
type Edge t = (Node t, Node t, Int)
data Graph t = NilG
               | Graph [Node t] [Edge t]

cNode :: (Eq t) => [Node t] -> Node t -> Bool
cNode nodes n | nodes == [] = False
              | (head nodes) == n = True
              | otherwise = cNode (tail nodes) n

eNode :: (Eq t) => [Node t] -> [Node t] -> Bool
eNode [] [] = True
eNode n1 n2 | n1 == [] || n2 == [] = False
            | (tail n2) == [] = cNode n1 (head n2)
            | otherwise = (cNode n1 (head n2)) && (eNode n1 (tail n2))

cEdge :: (Eq t) => [Edge t] -> Edge t -> Bool
cEdge edges e | edges == [] = False
              | (head edges) == e = True
              | otherwise = cEdge (tail edges) e

eEdge :: (Eq t) => [Edge t] -> [Edge t] -> Bool
eEdge [] [] = True
eEdge e1 e2 | e1 == [] || e2 == [] = False
            | (tail e2) == [] = cEdge e1 (head e2)
            | otherwise = (cEdge e1 (head e2)) && (eEdge e1 (tail e2))

equal :: (Eq t) => Graph t -> Graph t -> Bool
equal (NilG) (NilG) = True
equal _ (NilG) = False
equal (NilG) _ = False
equal (Graph n1 e1) (Graph n2 e2) = (eNode n1 n2) && (eNode n2 n1) && (eEdge e1 e2) && (eEdge e2 e1)

showNode :: (Eq t, Show t) => [Node t] -> String
showNode [] = []
showNode n = if tail n == [] then show (head n)
             else show (head n) ++ " " ++ showNode (tail n)

showEdge :: (Eq t, Show t) => [Edge t] -> String
showEdge [] = []
showEdge ((t1, t2, d):es) = if es == [] then "(" ++ (show t1) ++ " " ++ (show t2) ++ " " ++ (show d) ++ ")"
                                    else "(" ++ (show t1) ++ " " ++ (show t2) ++ " " ++ (show d) ++ ") " ++ showEdge es

showGraph :: (Eq t, Show t) => Graph t -> String
showGraph (NilG) = ""
showGraph (Graph n e) = "Nodes: (" ++ showNode n ++ ") Edges: (" ++ showEdge e ++ ")"

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
