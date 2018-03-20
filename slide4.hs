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

type Person = String
type Book = String
type Database = [(Person, Book)]
exampleDatabase :: Database
exampleDatabase = [("Sergio","O Senhor dos Aneis"), 
                   ("Andre","Duna"), 
                   ("Fernando","Jonathan Strange & Mr.Norrell"), 
                   ("Fernando","Duna"),
                   ("Fernando", "O Minimo Que Voce Precisa Saber Para Nao Ser Um Idiota")]

books :: Database -> Person -> [Book]
books [] argperson = []
books ((person, book): tail) argperson | person == argperson = [book] ++ (books tail argperson) 
                                       | otherwise = books tail argperson

booksFromDB:: Database -> [Book]
booksFromDB ((person, book): []) = [book]
booksFromDB ((person, book): tail) = [book] ++ (booksFromDB tail)

rental :: Database -> Book -> [Person]
rental [] argbook = []
rental ((person, book): tail) argbook  | book == argbook = [person] ++ (rental tail argbook) 
                                       | otherwise = rental tail argbook

rented :: Database -> Book -> Bool
rented [] argbook = False
rented ((person, book): tail) argbook  | book == argbook = True 
                                       | otherwise = rented tail argbook

qtyRented :: Database -> Person -> Int
qtyRented [] argperson = 0
qtyRented ((person, book): tail) argperson | person == argperson = 1 + (qtyRented tail argperson) 
                                           | otherwise = qtyRented tail argperson

renting :: Database -> Person -> Book -> Database
renting db person book = ((person, book):db)

devolution :: Database -> Person -> Book -> Database
devolution [] p b = []
devolution ((person, book): tail) p b | (p == person && b == book) = tail
                                      | otherwise = ((person, book) : (devolution tail p b))

unitaryList :: [Int] -> [Int]
unitaryList list = [x `div` x | x <- list]

memberComprehensive :: [Int] -> Int -> Bool
memberComprehensive list n = [x | x <- list, x == n] == (n:[])

booksComprehensive :: Database -> Person -> [Book]
booksComprehensive db seeked_person = [book | (dbperson, book) <- db, dbperson == seeked_person]

rentalComprehensive :: Database -> Book -> [Person]
rentalComprehensive db book = [person | (person, dbbook) <- db, dbbook == book]

rentedComprehensive :: Database -> Book -> Bool
rentedComprehensive db seeked_book = [book1 | (person, book1) <- db, book1 == seeked_book] /= []

qtyRentedComprehensive :: Database -> Person -> Int
qtyRentedComprehensive db seeked_person = length [book | (dbperson, book) <- db, dbperson == seeked_person]

devolutionComprehensive :: Database -> Person -> Book -> Database
devolutionComprehensive db person book = [(dbperson, dbbook) | (dbperson, dbbook) <- db, (dbperson, dbbook) /= (person, book)]

quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (head: tail) = (quickSort [left | left <- tail, left <= head]) ++ [head] ++ (quickSort [right | right <- tail, right > head])
