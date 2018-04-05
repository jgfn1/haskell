data Insctruction = PUSH Int | POP | ADD | SUB | DUP
type Stack = [Int]

data Weather = Cold | Hot

evalI :: Insctruction -> Stack -> Stack
evalI (PUSH x) [] = x:[]
evalI (PUSH x) stack = x:stack
evalI (POP) (a:as) = as
evalI (ADD) (a:b:as)  = ((a + b):as)
evalI (SUB) (a:b:as)  = ((a - b):as)
evalI (DUP) (a:as) = (a:a:as)

{-evalProg :: [Insctruction] -> Stack
evalProg [] = []
evalProg [x] = evalI x []
evalProg (a:as) = evalProg (reverse as) ++ evalI a (reverse as)-}

evalProg :: [Insctruction] -> Stack
evalProg i = evalProgAux i []

evalProgAux :: [Insctruction] -> Stack -> Stack
evalProgAux [] stack = stack
evalProgAux [i] stack = evalI i stack
evalProgAux (a:as)  stack = evalProgAux as (evalI a stack) 