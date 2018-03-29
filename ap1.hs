member :: [Int] -> Int -> Bool
member [] n = False
member (head:tail) n | head == n = True
                     | otherwise = member tail n

quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (head: tail) = (quickSort [left | left <- tail, left <= head]) ++ [head] ++ (quickSort [right | right <- tail, right > head])

kSmallest :: [Int] -> Int -> [Int]
kSmallest [] k = []
kSmallest list k = [x | x <- list, member (take k (quickSort list)) x]

isProductOfListElementsOnly :: [Int] -> Int -> Bool
isProductOfListElementsOnly _ 0 = False
isProductOfListElementsOnly _ 1 = True
isProductOfListElementsOnly [] x = False
isProductOfListElementsOnly (a:as) x = (((x `mod` a) == 0) && isProductOfListElementsOnly as (x `div` a)) || (((x `mod` a) == 0) && isProductOfListElementsOnly [a] (x `div` a)) || (isProductOfListElementsOnly as x)  

composites :: [Int] -> [Int] -> [Int]
composites [] _ = []
composites _ [] = []
composites primes list = [x | x <- list, isProductOfListElementsOnly primes x]