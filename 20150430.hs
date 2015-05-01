import Control.Monad

data Failable t = Error String | Value t deriving (Show)

instance Monad Failable where
 (>>=) (Error x) _ = Error x
 (>>=) (Value x) f = f x
 return x = (Value x)

data Fila t = NilF | Node t (Fila t) deriving (Show)

-- criarFila
criarFila :: Int -> t -> Failable (t, Fila t)
criarFila x f = if x <= 0 then Error "Tamanho incompativel"
                else Value (f, (Node f (NilF)))

-- push
isFull :: Fila t -> Int -> Bool
isFull (NilF) l = if l < 0 then True
                  else False
isFull (Node x f) l = isFull f (l - 1)

push :: Int -> t -> Fila t -> Failable (t, Fila t)
push l x (NilF) = criarFila l x
push l x f = if (isFull f (l - 1)) then Error "Lista cheia"
             else return (x, (Node x f))

-- pop
pop :: Fila t -> Failable (t, Fila t)
pop (NilF) = Error "Lista vazia"
pop (Node x f) = return (x, f)

-- peek (o que é que PEEK faz? '-')
peek :: Fila t -> Failable (t, Fila t)
peek (NilF) = Error "Lista vazia"
peek (Node x f) = return (x, (Node x f))