data Shape = Circle Float | Triangle Float Float

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Triangle a b) = a * b

data Expr = Lit Int | Add Expr Expr | Sub Expr Expr 

showExpr :: Expr -> String
showExpr (Lit n) = show n
showExpr (Add a b) = "(" ++ (showExpr a) ++ "+" ++ (showExpr b) ++ ")"
showExpr (Sub a b) = "(" ++ (showExpr a) ++ "-" ++ (showExpr b) ++ ")" 

data List t = Nil | Cons t (List t)
              deriving (Show)

toList :: List t -> [t]
toList Nil = []
toList (Cons a as) = (a:toList as)

fromList :: [t] -> List t
fromList [] = Nil
fromList (a:as) = Cons a (fromList as)

data Tree t = NilT | Node t (Tree t) (Tree t)
              deriving (Show)

depth :: Tree t -> Int
depth NilT = 0
depth (Node x NilT NilT) = 1
depth (Node x right left) = 1 + (depth right) + (depth left)

collapse :: Tree t -> [t]
collapse NilT = []
collapse (Node x right left) = [x] ++ collapse right ++ collapse left

mapTree :: (t -> u) -> Tree t -> Tree u
mapTree f NilT = NilT
mapTree f (Node x right left) = Node (f x) (mapTree f right) (mapTree f left)