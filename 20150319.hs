-- double
double :: [Int] -> [Int]
double [] = []
double x = (2 * (head x)) : double (tail x)

-- membership
membership :: [Int] -> Int -> Bool
membership l x | l == [] = False
               | (head l) == x = True
               | otherwise = membership (tail l) x

-- sumPairs
sumPairs :: [Int] -> [Int] -> [Int]
sumPairs x y | (null x || null y) = []
             | otherwise = ((head x) + (head y)) : sumPairs (tail x) (tail y)

-- digits
digits :: String -> String
digits x | x == [] = []
         | ((head x) >= '0' && (head x) <= '9') = [(head x)] ++ digits (tail x)
         | otherwise = digits (tail x)

-- quicksort
quicksort :: [Int] -> [Int]
quicksort x | x == [] = []
            | otherwise = quicksort [y | y <- (tail x), y < (head x)] ++ [head x] ++ quicksort [y | y <- (tail x), y >= (head x)]