-- Universidade Federal de Pernambuco
-- Centro de Informática (CIn)
-- Alunos: Victor Sin Yu Chen & Lucas de Souza Albuquerque
-- Login: vsyc & lsa2

---------------------------------------------------------------------------------------------

-- Trabalho
{-Trabalho 4, Questão 1-}
	{- Questão Teórica / Overloading -}

{-Em Haskell, onde se é utilizada a classe de tipos, a sobrecarga de função pode ser utilizada em tipos que não necessariamente são subtipos.
Ex.:
method :: (Eq a) => a -> a -> Bool
a poderia ser do tipo Int ou Char, ou até mesmo String, e o corpo do method não precisaria ser alterado se fosse necessário a chamada do método method para int e logo depois para String. A única restrição de tipos é que ambos os tipos necessitam fazer parte da classe de tipos definida no tipo do método.

Em Java é utilizada classe de objetos, onde a sobrecarga de uma função é realizada por meio da criação de vários métodos, de mesmo nome mas de assinaturas diferentes, que estão ou não numa mesma classe. Ao chamar o método, Java procura pela assinatura do método qual destes melhor se adapta ao contexto (tipo e número de parâmetros) e utiliza este para compilar.
Ex.:
public int sum (int a, int b){
    return a + b;
}

public double sum (double a, double b) {
    return a + b
}

public static void main (String args []){
    int ia = 1;
	int ib = 2;
	System.out.println(a+b); // o sistema escolheria a primeira função
	double da = 1.0;
	double db = 2.0;
	System.out.println(a+b); // o sistema escolheria a segunda função
}-}

---------------------------------------------------------------------------------------------

{-Trabalho 4, Questão 2-}
	{-Sequência Look and Say-}
	
--loop para calcular cada valor incremental da sequência
lookAndSay :: Int -> Int
lookAndSay 0 = 0
lookAndSay n | n == 1 = 1
             | otherwise = lookAndSayN (lookAndSay (n-1)) 1 0

--para calcular valores individuais
lookAndSayN :: Int -> Int -> Int -> Int
lookAndSayN 0 curr count = (10 * count) + curr
lookAndSayN n curr count | mod n 10 == curr = lookAndSayN (div n 10) curr (count + 1)
                         | otherwise = 100 * lookAndSayN (div n 10) (mod n 10) 1 + (10 * count) + curr
						 
---------------------------------------------------------------------------------------------
						 	
{-Trabalho 4, Questão 3-}
	{-Grafo-}					 
						 
type No = (Int, [Int])
type Grafo = [No]
 
caminho :: Grafo -> Int -> Int -> [Int]
caminho [] a b = []
caminho g a b | a == b = [a]
              | otherwise = shorten (loopCaminho g (generateBool(g)) a b (length(snd(g!!(a-1)))))
 
shorten :: [Int] -> [Int]
shorten [] = []
shorten g | g!!0 == g!!1 = drop 1 g
          | otherwise = g
 
loopCaminho :: Grafo -> [Bool] -> Int -> Int -> Int -> [Int]
loopCaminho [] v a b n = []
loopCaminho g v a b 0 = []
loopCaminho g v a b n | b == ((snd(g!!(a-1)))!!(n-1)) = [a, b]
                      | v!!((snd(g!!(a-1)))!!(n-1)) == False && loopCaminho g (atualizeV v ((snd(g!!(a-1)))!!(n-1))) ((snd(g!!(a-1)))!!(n-1)) b (length(snd(g!!((snd(g!!(a-1)))!!(n-1))))) /= [] = [a] ++ loopCaminho g (atualizeV v ((snd(g!!(a-1)))!!(n-1))) ((snd(g!!(a-1)))!!(n-1)) b (length(snd(g!!((snd(g!!(a-1)))!!(n-1)))))
					  | v!!((snd(g!!(a-1)))!!(n-1)) == False && loopCaminho g v a b (n-1) /= [] = [a] ++ loopCaminho g v a b (n-1)
                      | otherwise = []
                   
generateBool :: Grafo -> [Bool]
generateBool [] = []
generateBool (a:as) = False : generateBool(as)
 
atualizeV :: [Bool] -> Int -> [Bool]
atualizeV [] n = []
atualizeV ba n = (take n ba) ++ [True] ++ (drop (n+1) ba)						 
						 
---------------------------------------------------------------------------------------------

{-Trabalho 4, Questão 4-}
	{-Filtro Mediana-}
	
--"base", dá o tamanho do kernel
filtroMediana :: [[Int]] -> Int -> [[Int]]
filtroMediana [[]] n = [[]]
filtroMediana mt 1 = mt
filtroMediana mt n = medianaRows (mt) (div n 2) (length mt)

--varre as linhas da matriz
medianaRows :: [[Int]] -> Int -> Int -> [[Int]]
medianaRows [[]] n m = [[]]
medianaRows mt 0 m = mt
medianaRows mt n 0 = []
medianaRows mt n m = medianaColumns mt n ((length mt)-m) (length mt) : medianaRows mt n (m-1)

--varre as colunas da matriz, por linha
medianaColumns :: [[Int]] -> Int -> Int -> Int -> [Int]
medianaColumns [[]] n i m = []
medianaColumns mt 0 i m = mt!!i
medianaColumns mt n i 0 = []
medianaColumns mt n i m | (i <= n-1 || i == ((length mt)-n)) = mt!!i
                        | otherwise = (medianaSpot mt n i ((length mt)-m)) : (medianaColumns mt n i (m-1))
					
--chama a função para cada posição válida da matriz				
medianaSpot :: [[Int]] -> Int -> Int -> Int -> Int
medianaSpot [[]] n i j = 0
medianaSpot mt 0 i j = (mt!!i)!!j
medianaSpot mt n i j | (j <= n-1 || j == ((length mt)-n)) = (mt!!i)!!j
                     | otherwise = mediana(quicksort(vectorColumns mt n n i j))

-- varre as colunas ao redor do ponto para montar o vetor
vectorColumns :: [[Int]] -> Int -> Int -> Int -> Int -> [Int]
vectorColumns [[]] n m i j = []
vectorColumns mt n m i j | m + n == 0 = vectorRows mt m m i (j-n)
                         | otherwise = (vectorRows mt m m i (j-n)) ++ (vectorColumns mt (n-1) m i j)

-- varre as linhas ao redor do ponto para montar o vetor
vectorRows :: [[Int]] -> Int -> Int -> Int -> Int -> [Int]
vectorRows [[]] n m i j = []
vectorRows mt n m i j | m + n == 0 = (mt!!(i-n))!!j : []
                      | otherwise = ((mt!!(i-n))!!j) : (vectorRows mt (n-1) m i j)

-- quicksort para ordenação
quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (pivot:rest) = (quicksort [y | y <- rest, y < pivot]) ++ [pivot] ++ (quicksort [y | y <- rest, y >=  pivot])
					  
-- calcula a mediana de um vetor
mediana :: [Int] -> Int
mediana [] = 0
mediana ls | mod (length ls) 2 == 0 = div(ls!!(div (length ls) 2) + ls!!((div (length ls) 2) - 1)) 2
           | otherwise = ls!!(div (length ls) 2)

-- Exercícios
-- verifyAFD
getFst :: (Int, Int, Char) -> Int
getFst (x,y,z) = x

getSnd :: (Int, Int, Char) -> Int
getSnd (x,y,z) = y

getTrd :: (Int, Int, Char) -> Char
getTrd (x,y,z) = z

getTransition :: [(Int, Int, Char)] -> Int -> Char -> Int
getTransition t i s | t == [] = -1
                    | (getFst (head t)) == i && (getTrd (head t)) == s = getSnd (head t)
                    | otherwise = getTransition (tail t) i s

check :: [Int] -> Int -> Bool
check x y | x == [] = False
          | (head x) == y = True
          | otherwise = check (tail x) y

verifyAFD :: String -> [Int] -> [(Int, Int, Char)] -> Int -> [Int] -> Bool
verifyAFD s e t i f | s == "" = (check f i)
                    | otherwise = verifyAFD (tail s) e t (getTransition t i (head s)) f

-- somatorioHexadecimal
baseHD :: [(Char, Int)]
baseHD = [('0',0),('1',1),('2',2),('3',3),('4',4),('5',5),('6',6),('7',7),('8',8),('9',9),('A',10),('B',11),('C',12),('D',13),('E',14),('F',15)]

pow :: Int -> Int -> Int
pow x y | y == 0 = 1
        | otherwise = x * pow x (y - 1)

divHex :: Int -> Int
divHex x | x < 16 = 0
         | otherwise = 1 + (divHex (x - 16))

modHex :: Int -> Int
modHex x | x < 16 = x
         | otherwise = modHex (x - 16)

getDec :: [(Char, Int)] -> Char -> Int
getDec base x | base == [] = -1
              | (fst (head base)) == x = (snd (head base))
              | otherwise = getDec (tail base) x

getHex :: [(Char, Int)] -> Int -> Char
getHex base x | (snd (head base)) == x = (fst (head base))
              | otherwise = getHex (tail base) x

hexToDec :: String -> Int -> Int
hexToDec x y | x == [] = 0
             | otherwise = ((getDec baseHD (last x)) * (pow 16 y)) + hexToDec (init x) (y + 1)

decToHex :: Int -> String
decToHex x | x == 0 = []
           | otherwise = (decToHex (divHex x)) ++ [getHex baseHD (modHex x)]

sum :: [Int] -> Int
sum x | x == [] = 0
      | otherwise = (head x) + Main.sum (tail x)

aux :: [String] -> [Int]
aux x | x == [] = []
      | otherwise = [hexToDec (head x) 0] ++ aux (tail x)

somatorioHexadecimal :: [String] -> String
somatorioHexadecimal x | x == [] = []
                       | otherwise = decToHex (Main.sum (aux x))

-- palindromoDecimal
-- Obs.: Utiliza baseHD, pow e hexToDec do método acima
baseDD :: [(Char, Int)]
baseDD = [('0',0),('1',1),('2',2),('3',3),('4',4),('5',5),('6',6),('7',7),('8',8),('9',9)]

getInt :: [(Char, Int)] -> Char -> Int
getInt base x | base == [] = -1
              | (fst (head base)) == x = (snd (head base))
              | otherwise = getInt (tail base) x

getChar :: [(Char, Int)] -> Int -> Char
getChar base x | (snd (head base)) == x = (fst (head base))
               | otherwise = Main.getChar (tail base) x

intToString :: Int -> String
intToString x | x < 10 = [Main.getChar baseDD x]
              | otherwise = (intToString (div x 10)) ++ [Main.getChar baseDD (mod x 10)]

isPal :: String -> Bool
isPal x = if (length x) <= 1 then True
          else if (head x) /= (last x) then False
          else True && isPal (tail (init x))

palindromoDecimal :: String -> String
palindromoDecimal x = if (x == []) then []
                      else if (isPal decX == False) then decX ++ " - NAO-PALINDROMO"
                      else decX ++ " - PALINDROMO"
                           where decX = (intToString (hexToDec x 0))

-- multiplicaMatrizes
type Vector = [Double]
type Matrix = [Vector]

multElement :: Matrix -> Matrix -> Int -> Int -> Int -> Double
multElement m n a b c | m == [] || n == [] || b == -1 = 0
                      | otherwise = (((m!!a)!!b) * ((n!!b)!!c)) + (multElement m n a (b - 1) c)

multRow :: Matrix -> Matrix -> Int -> Int -> Vector
multRow m n x y | m == [] || n == [] || y == (length m) = []
                | otherwise = [multElement m n x (length m - 1) y] ++ (multRow m n x (y + 1))

auxMatrix :: Matrix -> Matrix -> Int -> Matrix
auxMatrix m n x | m == [] || n == [] || x == (length m) = []
          | otherwise = [multRow m n x 0] ++ (auxMatrix m n (x + 1))

multiplicaMatrizes :: Matrix -> Matrix -> Matrix
multiplicaMatrizes a b | (length a) == 0 || (length b) == 0 = []
                       | otherwise = auxMatrix a b 0