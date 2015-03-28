{-	Universidade Federal de Pernambuco
	Centro de Informática (CIn)
	Alunos: Victor Sin Yu Chen & Lucas de Souza Albuquerque
	Login: vsyc & lsa2
-}

-- Projeto
-- hashTable

{- Versão com HASH FINITA, COM SUBSITUIÇÃO EM CONFLITO
type HashTable = [(Int, Int)]

-- get
get :: HashTable -> Int -> Int
get [] kk = -1
get ht kk | fst(ht!!(mod kk 10)) == kk = snd(ht!!(mod kk 10))
          | otherwise = -1

-- put
put :: HashTable -> Int -> Int -> HashTable
put [] kk vv = [(kk, vv)]
put ht kk vv = take (mod kk 10) ht ++ (kk, vv) : drop ((mod kk 10)+1) ht

-- remove
remove :: HashTable -> Int -> HashTable
remove [] kk = []
remove ht kk | fst(ht!!(mod kk 10)) == kk = take (mod kk 10) ht ++ (-1, -1) : drop ((mod kk 10)+1) ht
             | otherwise = ht

-- hasKey
hasKey :: HashTable -> Int -> Bool
hasKey [] kk = False
hasKey ht kk = fst(ht!!(mod kk 10)) == kk
-}

-- Versão com HASH FINITA, FUNÇÃO DE MÓDULO usando LINEAR PROBING, ATUALIZAÇÃO e DESLOCAMENTO-}
type HashTable = [(Int, Int)]

baseExemplo :: HashTable
baseExemplo = [(1,4),(11,4),(3,6),(4,2),(9,5),(8,3),(7,-1),(12,8),(16,3),(10,2)]

-- question methods
-- hasKey
hasKey :: HashTable -> Int -> Bool
hasKey [] kk = False
hasKey ht kk = posGet ht kk 0 0 /= -1

-- get
get :: HashTable -> Int -> Int
get [] kk = -1
get ht kk | posGet ht kk 0 0 == -1 = -1
          | otherwise = snd(ht!!posGet ht kk 0 0)

-- put
put :: HashTable -> Int -> Int -> HashTable
put [] kk vv = [(kk,vv)]
put ht kk vv | posPut ht kk 0 0 == -1 = ht
             | otherwise = Prelude.take (posPut ht kk 0 0) ht ++ (kk, vv) : Prelude.drop ((posPut ht kk 0 0)+1) ht

-- remove
remove :: HashTable -> Int -> HashTable
remove [] kk = []
remove ht kk | posGet ht kk 0 0 == -1 = ht
             | otherwise = Prelude.take (posGet ht kk 0 0) ht ++ (-1,-1) : Prelude.drop ((posGet ht kk 0 0)+1) ht

-- find-position methods
posPut :: HashTable -> Int -> Int -> Int -> Int
posPut [] kk mm x = 0
posPut ht kk mm x | x >= 20 = -1
                  | fst(ht!!(mod ((mod kk 10) + mm) 10)) == kk = mod ((mod kk 10) + mm) 10
			      | x >= 10 && snd(ht!!(mod ((mod kk 10) + mm) 10)) == -1 = mod ((mod kk 10) + mm) 10
                  | otherwise = posPut ht kk (mm + 3) (x + 1)

posGet :: HashTable -> Int -> Int -> Int -> Int
posGet [] kk mm x = 0
posGet ht kk mm x | x >= 10 = -1
                  | fst(ht!!(mod ((mod kk 10) + mm) 10)) == kk = mod ((mod kk 10) + mm) 10
                  | otherwise = posGet ht kk (mm + 3) (x + 1)

-- comparaConjuntos
comparaConjuntos :: (Eq t) => [t] -> [t] -> String
comparaConjuntos a b | contem a b && contem b a = "A igual a B"
                     | contem b a = "A contem B"
                     | contem a b = "B contem A"
                     | intersecao a b = "A interseciona B"
					 | otherwise = "Conjuntos disjuntos"

-- verifica se todos os membros de A estão em B
contem :: (Eq t) => [t] -> [t] -> Bool
contem a b | a == [] = True
           | otherwise = (existe b (head a) && contem (tail a) b)

-- verifica se algum membro de A está em B
intersecao :: (Eq t) => [t] -> [t] -> Bool
intersecao a b | a == [] = False
               | otherwise = (existe b (head a) || intersecao (tail a) b)

-- verifica se existe um valor X em um conjunto
existe :: (Eq t) => [t] -> t -> Bool
existe xs s | xs == [] = False
            | otherwise = ((head xs) == s || existe (tail xs) s)


-- Exercícios
-- take
take :: (Eq t) => [t] -> Int -> [t]
take x y | x == [] || y == 0 = []
         | otherwise = (head x) : Main.take (tail x) (y-1)

-- drop
drop :: (Eq t) => [t] -> Int -> [t]
drop x y | x == [] = []
         | x /= [] && y > 0 = Main.drop (tail x) (y-1)
         | otherwise = (head x) : Main.drop (tail x) y

-- takeWhile
takeWhile :: (Eq t) => (t -> Bool) -> [t] -> [t]
takeWhile x y | y == [] = []
              | x (head y) = (head y) : Main.takeWhile x (tail y)
              | otherwise = []

-- dropWhile
dropWhile :: (Eq t) => (t -> Bool) -> [t] -> [t]
dropWhile x y | y == [] = []
              | x (head y) == False = Main.dropWhile x (tail y)
              | otherwise = (Main.drop y 0)

-- order
order :: (Ord a) => [a] -> [a]
order x | x == [] = []
        | otherwise = (order [minor | minor <- (tail x), minor < (head x)]) ++ [head x] ++ (order [major | major <- (tail x), major >= (head x)])

{- Método a ser resolvido.
-- agrupar
toString :: (Show a, Eq a) => [a] -> String
toString x |  x == [] = []
           | otherwise = show (head x) ++ (toString (tail x))

count :: String -> Char -> Int
count x y | x == [] = 0
          | (head x) == y = 1 + (count (tail x) y)
          | otherwise = count (tail x) y

removeChar :: String -> Char -> String
removeChar x y | x == [] = []
               | (head x) == y = removeChar (tail x) y
               | otherwise = (head x) : removeChar (tail x) y

agrupar :: (Ord a, Show a) => [a] -> [(Char,Int)]
agrupar x | x == [] = []
          | otherwise = let arr = toString x
		                in ((head arr), (count arr (head arr))) : agrupar (removeChar arr (head arr))
-}