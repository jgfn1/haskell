double :: [Int] -> [Int]
double [] = []
double (head:tail) = ((head * 2):(double tail))

member :: [Int] -> Int -> Bool
member [] n = False
member (head:tail) n | head == n = True
                     | otherwise = member tail n

isDigit :: Char -> Bool
isDigit n = (n >= '0') && (n <= '9')

digits :: String -> String
digits [] = []
digits (head:tail) | (isDigit head) = (head:(digits tail))
                   | otherwise = digits tail

sumPairs :: [(Int,Int)]->[Int]
sumPairs [] = []
sumPairs ((head1, head2):tail) = ((head1 + head2):(sumPairs tail))