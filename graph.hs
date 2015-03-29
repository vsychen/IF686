{- Defina um tipo de dados que representa um grafo não necessariamente conexo, onde cada nó tem um rótulo. Em seguida, defina uma função "search" que, dados um GRAFO, o RÓTULO DE UM NÓ INICIAL e o RÓTULO DE UM NÓ A SER ENCONTRADO, devolve um caminho (uma lista de arestas) entre o nó inicial e o nó a ser encontrado, se tal caminho existir. Você não deve restringir os rótulos dos nós a um tipo específico (embora todos os nós devam ter rótulos do mesmo tipo). Ao invés disso, seja tão geral quanto for possível. -}
-- Falta: REMODELAR TODA A FUNÇÃO

type Graph = [(Int, [Int])]
intGraphSample :: Graph
intGraphSample = [(1, [2,5]), (2, [1,3]), (3, [2,4]), (4, [3]), (5, [1])]

{-
type CharGraph = [(Char, [Char])]
charGraphSample :: CharGraph
charGraphSample = [('a', ['b','e']), ('b', ['a','c']), ('c', ['b','d']), ('d', ['c']), ('e', ['a'])]
-}

type Check = [(Int, Bool)]
checkSample :: Check
checkSample = [(1, False), (2, False), (3, False), (4, False), (5, False)]

getIndex :: Graph -> Int -> Int
getIndex [] _ = -100
getIndex (x:xs) index = if (fst x == index) then 0
                        else 1 + getIndex xs index

getPossible :: Check -> [Int]
getPossible [] = []
getPossible old = if snd (head old) == False then (fst (head old)) : getPossible (tail old)
                  else getPossible (tail old)

dListA :: [Int] -> Int -> Int
dListA [] _ = -1
dListA list x = if (head list) == x then x
                else dListA (tail list) x

dListB :: [Int] -> [Int] -> Int
dListB x y | x == [] || y == [] = -1
           | dListA x (head y) /= -1 = dListA x (head y)
           | otherwise = dListB x (tail y)

path :: Graph -> Check -> Int -> Int -> [Int]
path [] _ _ _ = []
path graph check init end = if (init < 0 || end < 0) then []
                            else if (init == end) then [end]
                            else if (newInit /= -1) then [init] ++ path graph newCheck newInit end
                            else []
                            where newCheck = ([x | x <- check, (fst x) < init] ++ [(init, True)] ++ [x | x <- check, (fst x) > init])
                                  newInit = dListB (getPossible check) (snd (graph!!(getIndex graph init)))


search :: Graph -> Int -> Int -> [Int]
search [] _ _ = []
search graph init end = path graph checkSample init end