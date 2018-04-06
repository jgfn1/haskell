delta = 1e-6

newton :: (Double -> Double) -> Double -> Double -> Double
newton f x0 eps | abs (x0 - (x0 - (f x0)/(f' f x0))) < eps = x0
                | otherwise = newton f (x0 - (f x0)/(f' f x0)) eps

f' :: (Double -> Double) -> Double -> Double
f' f x0 = (f (x0 + delta) - f(x0))/delta