import Control.Monad

data Failable t = Error String | Value t

instance Monad Failable where
 (>>=) (Error x) _ = Error x
 (>>=) (Value x) f = f x
 return x = (Value x)

data Fila t = NilF | Node t (Fila t)

criarFila :: Int -> t -> Failable (t, Fila t)
criarFila l f = 