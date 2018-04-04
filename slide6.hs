addnum :: Int -> (Int -> Int)
addnum n = (\m -> 3 + m)

f :: (Int -> Int -> Int) -> Int -> Int -> Int
f func a b = func a b

g :: (Int -> Int -> Int) -> Int -> Int -> Int
g func b a = func a b
	