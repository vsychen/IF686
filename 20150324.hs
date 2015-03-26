{-	Universidade Federal de Pernambuco
	Centro de Informática (CIn)
	Alunos: Victor Sin Yu Chen & Lucas de Souza Albuquerque
	Login: vsyc & lsa2
-}

-- Projeto
-- mergesort
{-	Complexidade:
	merge = N, onde N = (length list1) + (length list2)
	mergesort = log(N)

	Total = merge * mergesort = N*log(N)
-}
mergesort :: [Int] -> [Int]
mergesort a = if (length a) <= 1
              then a
              else merge (mergesort (take (div (length a) 2) a)) (mergesort (drop (div (length a) 2) a))

merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if (x <= y)
            then x : merge xs (y:ys)
			else y : merge (x:xs) ys

-- heapsort
{-	Complexidade: 
	swap = N
	large = CONSTANTE
	largest = CONSTANTE
	heapify = log(N)
	buildHeap: N*log(N), n chamadas sobre Heapify
	hpsort = N*log(N), na chamada de buildHeap
	heapsort = N*log(N), na chamada de hpsort
	
	Total = N*log(N)
-}
swap :: Int -> Int -> [Int] -> [Int]
swap i j xs | i == j    = xs
            | i < j     = (take i xs) ++ ((xs!!j):[]) ++ (drop (i+1) (take j xs)) ++ ((xs!!i):[]) ++ (drop (j+1) xs)
            | otherwise = (take j xs) ++ ((xs!!i):[]) ++ (drop (j+1) (take i xs)) ++ ((xs!!j):[]) ++ (drop (i+1) xs)

large :: Int -> Int -> [Int] -> Int
large i l xs = if ((2*i+1) < l) && ((xs!!(2*i+1)) > (xs!!i))
               then (2*i+1)
               else i

largest :: Int -> Int -> [Int] -> Int
largest i l xs = if ((2*i+2) < l) && ((xs!!(2*i+2)) > (xs!!(large i l xs)))
                 then (2*i+2)
                 else large i l xs

heapify :: Int -> Int -> [Int] -> [Int]
heapify i l xs = if ((largest i l xs) /= i)
                 then heapify (largest i l xs) l (swap (largest i l xs) i xs)
                 else xs

buildHeap :: Int -> [Int] -> [Int]
buildHeap i xs | i <= 0    = heapify 0 (length xs) xs
               | otherwise = buildHeap (i-1) (heapify i (length xs) xs)

hpsort :: Int -> [Int] -> [Int]
hpsort i xs = if (i == 1)
              then heapify 0 i (swap 0 i xs)
              else hpsort (i-1) (heapify 0 i (swap 0 i xs))

heapsort :: [Int] -> [Int]
heapsort xs | (length xs) <= 1 = xs
heapsort xs = hpsort ((length xs)-1) (buildHeap (div (length xs) 2) xs)

-- Exercícios
-- menorMaior
menorMaior :: Int -> Int -> Int -> (Int, Int)
menorMaior x y z | ((x > y) && (x < z)) || ((x < y) && (x > z)) = (y, z)
                 | ((y > x) && (y < z)) || ((y < x) && (y > z)) = (x, z)
                 | ((z > x) && (z < y)) || ((z < x) && (z > y)) = (x, y)
                 | otherwise = (-1, -1)

-- ordenaTripla (ordenacao MIN->MAX)
ordenaTripla :: (Int, Int, Int) -> (Int, Int, Int)
ordenaTripla (x, y, z) | (x >= y && y >= z) = (z, y, x)
                       | (x >= z && z >= y) = (y, z, x)
                       | (y >= x && x >= z) = (z, x, y)
                       | (y >= z && z >= x) = (x, z, y)
                       | (z >= x && x >= y) = (y, x, z)
                       | (z >= y && y >= x) = (x, y, z)

-- getX & getY
type Ponto = (Float, Float)

getX :: Ponto -> Float
getX p = (fst p)

getY :: Ponto -> Float
getY p = (snd p)

-- vertical
type Reta = (Ponto, Ponto)

vertical :: Reta -> Bool
vertical r = if (snd (fst r) == snd (snd r))
             then True
             else False

-- pontoY sera igual a 0

type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa, Livro)]

baseExemplo :: BancoDados
baseExemplo = [("Sergio","O Senhor dos Aneis"),("Andre","Duna"),("Fernando","Jonathan Strange & Mr. Norrell"), ("Fernando","A Game of Thrones")]

-- livros
livros :: BancoDados -> Pessoa -> [Livro]
livros [] _ = []
livros ((p,l):xs) y | y == p    = l:livros xs y
                    | otherwise = livros xs y

-- emprestimos
emprestimos :: BancoDados -> Livro -> [Pessoa]
emprestimos [] _ = []
emprestimos ((p,l):xs) y | y == l    = p:emprestimos xs y
                         | otherwise = emprestimos xs y

-- emprestado
emprestado :: BancoDados -> Livro -> Bool
emprestado [] _ = False
emprestado ((p,l):xs) y | y == l    = True
                        | otherwise = emprestado xs y

-- qtdEmprestimos
qtdEmprestimos :: BancoDados -> Pessoa -> Int
qtdEmprestimos [] _ = 0
qtdEmprestimos ((p,l):xs) y | y == p    = 1+qtdEmprestimos xs y
                            | otherwise = qtdEmprestimos xs y

-- emprestar
emprestar :: BancoDados -> Pessoa -> Livro -> BancoDados
emprestar x p l = (p,l):x

-- devolver
devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver x p l | x == []                                      = []
               | (p == (fst (head x)) && l == (snd (head x))) = devolver (tail x) p l
               | otherwise                                    = (head x):(devolver (tail x) p l)

-- membro
membroR :: [Int] -> Int -> Bool
membroR x y = [membro | membro <- x, membro == y] /= []

-- livros
livrosR :: BancoDados -> Pessoa -> [Livro]
livrosR x y = [livros | (pessoa, livros) <- x, pessoa == y]

-- emprestimos
emprestimosR :: BancoDados -> Livro -> [Pessoa]
emprestimosR x y = [pessoa | (pessoa, livro) <- x, livro == y]

-- emprestado
emprestadoR :: BancoDados -> Livro -> Bool
emprestadoR x y = [livro | (pessoa, livro) <- x, livro == y] /= []

-- qtdEmprestimos
qtdEmprestimosR :: BancoDados -> Pessoa -> Int
qtdEmprestimosR x y = (length [pessoa | (pessoa, livro) <- x, pessoa == y])

-- devolver
devolverR :: BancoDados -> Livro -> BancoDados
devolverR x y = [(pessoa, livro) | (pessoa, livro) <- x, livro /= y]

-- quicksort
quicksortR :: [Int] -> [Int]
quicksortR x | (length x) <= 1 = x
            | otherwise = (quicksortR [minor | minor <- (tail x), minor < (head x)]) ++ [head x] ++ (quicksortR [major | major <- (tail x), major >= (head x)])

-- getSpace
getSpace :: String -> Int
getSpace x | x == [] = 0
           | (head x) == ' ' = 0
           | otherwise = 1 + getSpace (tail x)

-- getWord
getWord :: String -> String
getWord x | x == [] = []
          | otherwise = take (getSpace x) x

-- dropWord
dropWord :: String -> String
dropWord x | x == [] = []
           | otherwise = drop (getSpace x) x

-- dropSpace
dropSpace :: String -> String
dropSpace x | x == [] = []
            | x!!0 /= ' ' = x
            | otherwise = dropSpace (tail x)

type Word = String

-- splitWords
splitWords :: String -> [Word]
splitWords x | x == [] = []
             | ((getSpace x) == 0 && (x!!0 /= ' ')) = (getWord x):[]
             | otherwise = (getWord x) : (splitWords (dropSpace (dropWord x)))

type Line = [Word]

-- getLine
getLine :: Int -> [Word] -> Line
getLine x y | x == 0 || y == [] = []
            | length (head y) > x = Main.getLine x (tail y)
            | otherwise = (head y) : Main.getLine (x - (length (head y))) (tail y)

-- dropLine
dropLine :: Int -> [Word] -> [Word]
dropLine x y | x == 0 || y == [] = []
             | length (last y) > x = dropLine x (init y)
             | otherwise = (dropLine (x - (length (last y))) (init y)) ++ [(last y)]

lineLength = 42

-- splitLines
splitLines :: [Word] -> [Line]
splitLines x | x == [] = []
             | otherwise = (Main.getLine lineLength x) : splitLines (drop 10 x)

-- fill
fill :: String -> [Line]
fill x = splitLines (splitWords x)


-- joinLines
lineToWord :: [Line] -> [Word]
lineToWord x | x == [] = []
             | otherwise = (head x) ++ lineToWord (tail x)

wordToString :: [Word] -> String
wordToString x | x == [] = []
               | (tail x) == [] = (head x)
               | otherwise = (head x) ++ " " ++ wordToString (tail x)

joinLines :: [Line] -> String
joinLines x | x == [] = []
            | otherwise = wordToString (lineToWord x)