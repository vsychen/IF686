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

