-- Universidade Federal de Pernambuco
-- Centro de Informática (CIn)
-- Alunos: Victor Sin Yu Chen & Lucas de Souza Albuquerque
-- Login: vsyc & lsa2

---------------------------------------------------------------------------------------------

{-Trabalho 10, Questão 1-}
	{-Tipos-}
{-
1) foldr (+).(.).map

2) (\x y z -> foldr z x y).map --------------------------------------CHECAR------------------------------------------
foldr :: (a -> b -> b) -> b -> [a] -> b
z :: (a -> b -> b)
x :: b
y :: [a]
(\x y z -> foldr z x y) :: b -> [a] -> (a -> b -> b) -> b

(.) :: (d -> e) -> (c -> d) -> (c -> e)
map :: (f -> g) -> [f] -> [g]

- c = b
- d = ([a] -> (a -> b -> b) -> b)
- d = (f -> g)
- e = ([f] -> [g])

- f = [a]
- g = (a -> b -> b) -> b

(\x y z -> foldr z x y).map :: (c -> e)
(\x y z -> foldr z x y).map :: (b -> ([f] -> [g]))
(\x y z -> foldr z x y).map :: (b -> ([[a]] -> [(a -> b -> b) -> b]))

3) map.((.) (foldr (++) (foldr (++) [] [[1], [2]])))

4) (foldr).(.)$(!!)
-}