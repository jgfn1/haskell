-- Q1
delta = 1e-6

newton :: (Double -> Double) -> Double -> Double -> Double
newton f x0 eps | abs (x0 - (x0 - (f x0)/(f' f x0))) < eps = x0
                | otherwise = newton f (x0 - (f x0)/(f' f x0)) eps

f' :: (Double -> Double) -> Double -> Double
f' f x0 = (f (x0 + delta) - f(x0))/delta

-- Q2
{-
a) 
	(.) thrice map = thrice . map = thrice (map)
	Como a função de composição apenas aplica thrice à map, e thrice
	por sua vez apenas aplica a função passada como argumento três vezes
	ao seu próprio argumento, o tipo total é o mesmo de map, i.e.:
	( . ) thrice map :: (a -> b) -> [a] -> [b]

b)
	Como swap só recebe >uma< função como argumento, o tipo é indeterminado
	já que a composição com as outras funções não é possível. 

c)
    O resultado será uma lista vazia, que segundo o GHCI é do tipo [t]. Mas o
    tipo de entrada não pode ser determinado já que para qualquer lista
    o resultado seria []. O interpretador afirma que o tipo é [[a]] -> [a],
    no entanto a função pode ser aplicada a argumentos que não são necessa-
    riamente [[a]], por isso é inconclusivo.

-}

-- Q3
countOccur :: [String] -> String -> Int
countOccur [] _ = 0
countOccur list x = length [y | y <- list, y == x]

headsFromLists :: [[t]] -> [t]
headsFromLists [] = []
headsFromLists (a:as) = (head a):(headsFromLists as)

maxOccur:: [[String]] -> Int
maxOccur [] = 0
maxOccur list = max (countOccur (headsFromLists list) (head (headsFromLists list))) (maxOccur (tail list))

moreTimesInFst :: [[String]] -> String
moreTimesInFst (a:as) | countOccur (headsFromLists (a:as)) (head a) > countOccur

winner :: [[String]] -> String
winner [] = ""
winner list | (fstInMoreThanHalf (moreTimesInFst list) list) = moreTimesInFst list
winner list | 

{-
winner(list)
{
	movie = moreTimesInFst(list);
	while(fstInMoreThanHalf(movie, list) == false)
	{
		while(OnlyOneWithLessKthPositions(k, list) == false)
		{
			k++;
		}
		RemoveFromList(movieWithLessKthPositions(k, list));
		movie = moreTimesInFst(list);
	}
	winner = movie;
	return winner;
}
-}