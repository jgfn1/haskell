mapping :: (t -> t) -> [t] -> [t]
mapping f [] = []
mapping f list = [f x | x <- list]

sqList :: [Int] -> [Int]
sqList [] = []
sqList list = map (^2) list

folding :: (t -> t -> t) -> [t] -> t
folding f [a] = a 
folding f (a: as) = f a (folding f as)

sqSumList :: [Int] -> Int
sqSumList [] = 0
sqSumList list = folding (+) (sqList list)

filtering :: (t -> Bool) -> [t] -> [t]
filtering f [] = []
filtering f list = [x | x <- list, f x]

biggerThanZero :: [Int] -> [Int]
biggerThanZero [] = []
biggerThanZero list = filtering (>0) list

{-
Creates a new list in which each element of it is one element of the old list.
-}
unkown :: [t] -> [t]
unkown l = foldr (++) [] (map sing l)
           where sing a = [a]

biggest :: [[Int]] -> [Int]
biggest [] = []
biggest (a:as) = (folding (max) a) ++ biggest as