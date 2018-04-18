-- 1

-- 2
data Temperature = Celsius Float | Fahrenheit Float | Kelvin Float

instance Eq Temperature where
    (==) (Celsius x) (Kelvin y) = x == y - 273
    (==) (Kelvin y) (Celsius x) = x + 273 == y     
    (==) (Celsius x) (Fahrenheit y) = x/5 == ((y - 32) / 9)    
    (==) (Fahrenheit y) (Celsius x) = x/5 == (y - 32)*9    
    (==) (Kelvin x) (Fahrenheit y) = x == (((y - 32) / 9) * 5) + 273
    (==) (Fahrenheit y) (Kelvin x) = x == (((y - 32) / 9) * 5) + 273
    (/=) (Celsius x) (Kelvin y) = x /= y - 273
    (/=) (Kelvin y) (Celsius x) = x /= y - 273     
    (/=) (Celsius x) (Fahrenheit y) = x /= ((y - 32) / 9) * 5    
    (/=) (Fahrenheit y) (Celsius x) = x /= ((y - 32) / 9) * 5    
    (/=) (Kelvin x) (Fahrenheit y) = x /= (((y - 32) / 9) * 5) + 273
    (/=) (Fahrenheit y) (Kelvin x) = x /= (((y - 32) / 9) * 5) + 273
    -- (k - 273)/5 = (f - 32)/9    

instance Show Temperature where
    show (Celsius x) = (show x) ++ " °C"
    show (Fahrenheit x) = (show x) ++ " °F"
    show (Kelvin x) = (show x) ++ " K"

instance Ord Temperature where
    (<) (Celsius x) (Kelvin y) = x < (y - 273)
    (<) (Kelvin y) (Celsius x) = x < (y - 273)     
    (<) (Celsius x) (Fahrenheit y) = x < ((y - 32) / 9) * 5    
    (<) (Fahrenheit y) (Celsius x) = x/5 < ((y - 32) / 9)    
    (<) (Kelvin x) (Fahrenheit y) = x < ((((y - 32) / 9) * 5) + 273)
    (<) (Fahrenheit y) (Kelvin x) = (x - 273)/5 < (((y - 32) / 9))
    (<=) (Celsius x) (Kelvin y) = x <= (y - 273)
    (<=) (Kelvin y) (Celsius x) = x <= (y - 273)     
    (<=) (Celsius x) (Fahrenheit y) = x <= (((y - 32) / 9) * 5)    
    (<=) (Fahrenheit y) (Celsius x) = x <= (((y - 32) / 9) * 5)  
    (<=) (Kelvin x) (Fahrenheit y) = x <= ((((y - 32) / 9) * 5) + 273)
    (<=) (Fahrenheit y) (Kelvin x) = x <= ((((y - 32) / 9) * 5) + 273)
    (>) (Celsius x) (Kelvin y) = x > (y - 273)
    (>) (Kelvin y) (Celsius x) = x > (y - 273)     
    (>) (Celsius x) (Fahrenheit y) = x > ((((y - 32) / 9) * 5))    
    (>) (Fahrenheit y) (Celsius x) = x > ((((y - 32) / 9) * 5))  
    (>) (Kelvin x) (Fahrenheit y) = x > ((((((y - 32) / 9) * 5)) + 273))
    (>) (Fahrenheit y) (Kelvin x) = (x - 273)/5 > ((y - 32) / 9)
    (>=) (Celsius x) (Kelvin y) = x >= y - 273
    (>=) (Kelvin y) (Celsius x) = x >= y - 273     
    (>=) (Celsius x) (Fahrenheit y) = x >= (((y - 32) / 9) * 5)    
    (>=) (Fahrenheit y) (Celsius x) = x >= (((y - 32) / 9) * 5)  
    (>=) (Kelvin x) (Fahrenheit y) = x >= (((((y - 32) / 9) * 5)) + 273)
    (>=) (Fahrenheit y) (Kelvin x) = x >= (((((y - 32) / 9) * 5)) + 273)
    max x y | x > y = x
            | x <= y = y
    -- max x y = max x y
    {-max (Kelvin y) (Celsius x) = (x + 273) `max` y     
    max (Celsius x) (Fahrenheit y) = x `max` (((y - 32) / 9) * 5)    
    max (Fahrenheit y) (Celsius x) = (((x/5) * 9) + 32) `max` y    
    max (Kelvin x) (Fahrenheit y) = x `max` (((((y - 32) / 9) * 5)) + 273)
    max (Fahrenheit y) (Kelvin x) = ((((x - 273) / 5) * 9) + 32) `max` y
    min (Celsius x) (Kelvin y) = x `min` y - 273
    min (Kelvin y) (Celsius x) = (x + 273) `min` y     
    min (Celsius x) (Fahrenheit y) = x `min` (((y - 32) / 9) * 5)    
    min (Fahrenheit y) (Celsius x) = (((x/5) * 9) + 32) `min` y    
    min (Kelvin x) (Fahrenheit y) = x `min` (((((y - 32) / 9) * 5)) + 273)
    min (Fahrenheit y) (Kelvin x) = ((((x - 273) / 5) * 9) + 32) `min` y        -}