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

{-smallerBigger :: Int -> Int -> Int -> (Int, Int)
smallerBigger a b c | a > b && a > c && b > c = (c, a)
                    | otherwise a > b && a > c && c > b = (b, a)
                    | otherwise b > a && b > c && a > c = (c, b)
                    | otherwise b > a && b > c && c > a = (a, b)
                    |-}

smallerBigger :: Int -> Int -> Int -> (Int, Int)
smallerBigger a b c  | (a > c && b > c) = (c, (max a b))
                     | (a > b && c > b) = (b, (max a c))
                     | (b > a && c > a) = (a, (max b c))

orderTriple :: (Int, Int, Int) -> (Int, Int, Int)
orderTriple (a, b, c) | (a < (max b c) && a > (min b c)) = ((min b c) , a, (max b c))
                      | (b < (max a c) && b > (min a c)) = ((min a c) , b, (max a c))
                      | (c < (max b a) && c > (min b a)) = ((min b a) , c, (max b a))
 