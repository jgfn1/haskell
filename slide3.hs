addSpaces :: Int->String
addSpaces n | n == 0 = ""
            | otherwise = " " ++ addSpaces(n - 1)

shiftRight :: Int->String->String
shiftRight n str = (addSpaces n) ++ str

sales :: Int->Int
sales n = (n*10) + 100

totalSales :: Int->Int
totalSales n | n == 0 = sales 0
             | otherwise = (sales n) + (totalSales (n - 1))

maxSale :: Int->Int
maxSale n | n == 0 = sales 0
          | otherwise = max (sales n) (maxSale (n - 1))

tableSales :: Int->String
tableSales 0 = show(0) ++ shiftRight 8 (show (sales 0)) ++ "\n"
tableSales n = tableSales (n - 1) ++ show (n) ++
    shiftRight 8 (show (sales n)) ++ "\n"

printTable :: Int->IO()
printTable n = putStr ("Semana   Venda\n" ++ tableSales n)

smallerBigger :: Int -> Int -> Int -> (Int, Int)
smallerBigger a b c  | (a > c && b > c) = (c, (max a b))
                     | (a > b && c > b) = (b, (max a c))
                     | (b > a && c > a) = (a, (max b c))

orderTriple :: (Int, Int, Int) -> (Int, Int, Int)
orderTriple (a, b, c) | (a < (max b c) && a > (min b c)) = ((min b c) , a, (max b c))
                      | (b < (max a c) && b > (min a c)) = ((min a c) , b, (max a c))
                      | (c < (max b a) && c > (min b a)) = ((min b a) , c, (max b a))
 
type Point = (Float, Float)
type Line = (Point, Point)

fstCoordinate :: Point -> Float
fstCoordinate (a, b) = a

sndCoordinate :: Point -> Float
sndCoordinate (a, b) = b

isVertical :: Line -> Bool
isVertical (p1, p2) = (fstCoordinate(p1) == fstCoordinate(p2))

slope :: Line -> Float
slope ((x1, y1), (x2, y2)) = (y2 - y1)/(x2 - x1)

yPoint :: Float -> Line -> Float
yPoint x line = (slope line)*x