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
func (x:xs) l = if xs /= [] then [[min | min <- l, min <= x]] ++ func xs [maj | maj <- l, maj > x]
                else [[min | min <- l, min <= x]] ++ [[max | max <- l, max > x]]

listPartitioner :: [Int] -> ([Int] -> [[Int]])
listPartitioner [] = func []
listPartitioner x = func (quicksort x)

