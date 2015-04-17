-- Universidade Federal de Pernambuco
-- Centro de Informática (CIn)
-- Alunos: Victor Sin Yu Chen & Lucas de Souza Albuquerque
-- Login: vsyc & lsa2

---------------------------------------------------------------------------------------------

{-Trabalho 9, Questão 1-}
-- Slide da Aula 02
-- joinLines
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



