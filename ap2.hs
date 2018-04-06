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
	swap map thrice = thrice (map)
	Logo, o tipo é igual ao da letra a).

c)
    O resultado será uma lista vazia, e segundo o GHCI [] :: [t]. 
-}