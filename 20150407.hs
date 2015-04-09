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

