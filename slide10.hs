main :: IO()
main = do name <- getLine
          file <- readFile name
          putStr file
          -- putStr uniqueList file
          let elems = uniqueList file in putStrLn (format elems (concatListOfLists ([(show (countOccur file x)) | x <- elems, (elem x (['a'..'z'] ++ ['0'..'9']))])))

format :: String -> String -> String
format [] _ = ""
format _ [] = ""
format (a:as) (b:bs) | (elem a (['a'..'z'] ++ ['0'..'9'])) = [a] ++ " --> " ++ [b] ++ "\n" ++ format as bs
                     | otherwise = format as (b:bs)

countOccur :: String -> Char -> Int
countOccur [] _ = 0
countOccur list x = length [y | y <- list, y == x]

countOccurInt :: [Int] -> Int -> Int
countOccurInt [] _ = 0
countOccurInt list x = length [y | y <- list, y == x]

uniqueList :: String -> String
uniqueList [] = []
uniqueList (head:tail) | not (elem head tail) = [head]  ++ uniqueList tail
                       | otherwise = uniqueList tail

concatListOfLists :: [[t]] -> [t]
concatListOfLists [] = []
concatListOfLists (a:as) = a ++ concatListOfLists as