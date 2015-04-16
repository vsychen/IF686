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

-- 

-- sumX
sum :: (Num t) => t -> t -> t
sum x y = x + y

sumX :: (Num t) => t -> ([t] -> [t])
sumX n = map (Main.sum n)

-- maxEl
maxEl :: (Ord t) => ([t] -> t)
maxEl = maximum

-- 