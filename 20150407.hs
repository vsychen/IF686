-- area
data Shape = Circle Float
           | Rectangle Float Float

area :: Shape -> Float
area (Circle x) = 2*pi*(x)^2
area (Rectangle x y) = x*y

-- isWeekend
type HorasAula = Int
type Discip = [String]
data WeekDay = Segunda HorasAula Discip
             | Terca HorasAula Discip
             | Quarta HorasAula Discip
             | Quinta HorasAula Discip
             | Sexta HorasAula Discip
             | Sabado HorasAula Discip
             | Domingo

isWeekend :: WeekDay -> Bool
isWeekend (Sabado _ _) = True
isWeekend (Domingo) = True
isWeekend _ = False

-- hasPLC
temMateria :: Discip -> String -> Bool
temMateria x mat = if x == [] then False
                   else if (head x) == mat then True
                   else temMateria (tail x) mat

hasPLC :: WeekDay -> Bool
hasPLC (Domingo) = False
hasPLC (Segunda h disc) = temMateria disc "PLC"
hasPLC (Terca h disc) = temMateria disc "PLC"
hasPLC (Quarta h disc) = temMateria disc "PLC"
hasPLC (Quinta h disc) = temMateria disc "PLC"
hasPLC (Sexta h disc) = temMateria disc "PLC"
hasPLC (Sabado h disc) = temMateria disc "PLC"

-- tree
data Tree t = NilT | Node t (Tree t) (Tree t) deriving (Eq, Show)

-- showExpr
data Expr = Lit Int | Add Expr Expr | Sub Expr Expr

showExpr :: Expr -> String
showExpr (Lit n) = show n
showExpr (Add e1 e2) = "(" ++ (showExpr e1) ++ "+" ++ (showExpr e2) ++ ")"
showExpr (Sub e1 e2) = "(" ++ (showExpr e1) ++ "-" ++ (showExpr e2) ++ ")"

-- toList
data List t = Nil | Cons t (List t)

toList :: List t -> [t]
toList (Nil) = []
toList (Cons x (xs)) = x : toList xs

-- fromList
fromList :: [t] -> List t
fromList [] = Nil
fromList (x:xs) = Cons x (fromList xs)

-- depth
max :: Int -> Int -> Int
max x y = if (x >= y) then x
          else y

depth :: Tree t -> Int
depth (NilT) = 0
depth (Node t t1 t2) = 1 + Main.max (depth t1) (depth t2)

-- collapse
collapse :: Tree t -> [t]
collapse (NilT) = []
collapse (Node x (NilT) (NilT)) = [x]
collapse (Node x t1 (NilT)) = [x] ++ (collapse t1)
collapse (Node x (NilT) t2) = [x] ++ (collapse t2)
collapse (Node x t1 t2) = [x] ++ (collapse t1) ++ (collapse t2)

-- bsf
bsf :: (Ord t) => Tree t -> t -> Bool
bsf (NilT) _ = False
bsf (Node x a b) n = if x == n then True
                     else if n < x then bsf a n
                     else bsf b n

-- mapTree
mapTree :: (t -> u) -> Tree t -> Tree u
mapTree f (NilT) = NilT
mapTree f (Node x a b) = (Node (f x) (mapTree f a) (mapTree f b))