member :: [Int] -> Int -> Bool
member [] n = False
member (head:tail) n | head == n = True
                     | otherwise = member tail n

quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (head: tail) = (quickSort [left | left <- tail, left <= head]) ++ [head] ++ (quickSort [right | right <- tail, right > head])

kSmallest :: [Int] -> Int -> [Int]
kSmallest [] k = []
kSmallest list k = [x | x <- list, member (take k (quickSort (unique list))) x]

isProductOfListElementsOnly :: [Int] -> Int -> Bool
isProductOfListElementsOnly _ 0 = False
isProductOfListElementsOnly _ 1 = True
isProductOfListElementsOnly [] x = False
isProductOfListElementsOnly (a:as) x = (((x `mod` a) == 0) && isProductOfListElementsOnly as (x `div` a)) || (((x `mod` a) == 0) && isProductOfListElementsOnly [a] (x `div` a)) || (isProductOfListElementsOnly as x)  

composites :: [Int] -> [Int] -> [Int]
composites [] _ = []
composites _ [] = []
composites primes list = [x | x <- list, isProductOfListElementsOnly primes x]

unique :: [Int] -> [Int]
unique [] = []
unique (head:tail) | not (member tail head) = [head]  ++ unique tail
                   | otherwise = unique tail

remove :: [Int] -> Int -> [Int]
remove [] y = []
remove list y = [x | x <- list, x /= y]

memberList :: [[Int]] -> [Int] -> Bool
memberList [] _ = False
memberList [[]] [] = True
memberList (head:tail) [] | head == [] = True
                          | otherwise = memberList tail []
memberList (head:tail) n | head == n = True
                         | otherwise = memberList tail n

uniqueList :: [[Int]] -> [[Int]]
uniqueList [] = []
uniqueList [[]] = [[]]
uniqueList (head:tail) | not (memberList tail head) = [head]  ++ uniqueList tail
                       | otherwise = uniqueList tail

combinationsList :: [[Int]] -> [[Int]]
combinationsList [] = []
combinationsList (a:as) = combinations a ++ combinationsList as

combinations :: [Int] -> [[Int]]
combinations [] = [[]]
combinations list = [list] ++ uniqueList (combinationsList [(take i list ++ drop (i + 1) list) | i <- [0..((length list) - 1)]])