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

unique :: [Int] -> [Int]
unique [] = []
unique (head:tail) = [head | head <- (head:tail), not (member tail head)]  ++ unique tail

remove :: [Int] -> Int -> [Int]
remove [] y = []
remove list y = [x | x <- list, x /= y]

func :: [Int] -> [Int]
func [] = []
func (a:as) = [a] ++ func as

combinations :: [Int] -> [[Int]]
combinations [] = [[]]
combinations (a:as) = [drop i ([a] ++ func as) | i <- [0..length (a:as)]]