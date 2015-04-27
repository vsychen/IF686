-- Universidade Federal de Pernambuco
-- Centro de Informática (CIn)
-- Alunos: Victor Sin Yu Chen & Lucas de Souza Albuquerque
-- Login: vsyc & lsa2

---------------------------------------------------------------------------------------------

{-Trabalho 10, Questão 1-}
	{-Tipos-}
{-
1) foldr (+).(.).map
- foldr :: (a -> b -> b) -> b -> [a] -> b
- (+) :: Num c => c -> c -> c
- . :: (e -> f) -> (d -> e) -> (d -> f)
- (.) :: (h -> i) -> (g -> h) -> (g -> i)
- . :: (k -> l) -> (j -> k) -> (j -> l)
- map :: (m -> n) -> [m] -> [n]

---------- (.).map ----------
- j = (m -> n)
- k = (h -> i)
- k = ([m] -> [n])
- l = (g -> h) -> (g -> i)

- h = [m]
- i = [n]

((.).map) :: (j -> l)
((.).map) :: (m -> n) -> ((g -> h) -> (g -> i))
((.).map) :: (m -> n) -> ((g -> [m]) -> (g -> [n]))

---------- foldr (+) ----------
- a = c
- b = c

foldr (+) :: (Num c) => c -> [c] -> c

---------- (foldr (+)).((.).map) ----------
- d = (m -> n)
- e = ((g -> [m]) -> (g -> [n]))
- e = (Num c) => c
- f = (Num c) => [c] -> c

- c = ((g -> [m]) -> (g -> [n]))

(foldr (+)).((.).map) :: (d -> f)
(foldr (+)).((.).map) :: (Num c) => (m -> n) -> [c] -> c
(foldr (+)).((.).map) :: (Num ((g -> [m]) -> (g -> [n]))) => (m -> n) -> [(g -> [m]) -> (g -> [n])] -> ((g -> [m]) -> (g -> [n]))

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
[1] :: Num t -> [t]
[2] :: Num t -> [t]
(foldr (++) [] [[1], [2]]) :: (Num t) => [t]

- foldr :: (a -> b -> b) -> b -> [a] -> b
- b = [t]
- a = [t]

(foldr (++) (foldr (++) [] [[1], [2]])) :: [a] -> b
(foldr (++) (foldr (++) [] [[1], [2]])) :: (Num t) => [[t]] -> [t]

---------- (.) (foldr (++) (foldr (++) [] [[1], [2]])) ----------
(.) :: (b0 -> c0) -> (a0 -> b0) -> (a0 -> c0)

- a0 = t
- b0 = [[t]]
- c0 = [t]

(.) (foldr (++) (foldr (++) [] [[1], [2]])) :: (Num t) => (a0 -> b0) -> (a0 -> c0)
(.) (foldr (++) (foldr (++) [] [[1], [2]])) :: (Num t) => (t -> [[t]]) -> (t -> [t])

---------- map.((.) (foldr (++) (foldr (++) [] [[1], [2]]))) ----------
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

4) (foldr).(.)$(!!)
- foldr :: (a -> b -> b) -> b -> [a] -> b
- . :: (d -> e) -> (c -> d) -> (c -> e)
- (.) :: (g -> h) -> (f -> g) -> (f -> h)
- $ :: (i -> j) -> i -> j
- (!!) :: [k] -> Int -> k

---------- (foldr).(.) ----------
- c = (a -> b -> b)
- d = (b -> [a] -> b)
- d = (g -> h)
- e = (f -> g) -> (f -> h)

- g = b
- h = ([a] -> b)

(foldr).(.) :: (c -> e)
(foldr).(.) :: (a -> b -> b) -> ((f -> g) -> (f -> h))
(foldr).(.) :: (a -> b -> b) -> ((f -> b) -> (f -> ([a] -> b)))

---------- (foldr).(.)$(!!) ----------
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