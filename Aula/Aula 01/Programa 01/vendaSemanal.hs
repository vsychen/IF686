vendas :: Int -> Int
vendas x = x * x

funcao :: Int -> Int -> Int
funcao s n | ((n == 0) && (vendas n == s)) = 1
        | n == 0 = 0
        | (vendas n == s) = (funcao s (n-1)) + 1
        | otherwise = (funcao s (n-1))