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