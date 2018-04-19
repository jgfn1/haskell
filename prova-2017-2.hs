--
--Primeira Prova de Paradigmas de Linguagens Computacionais 
-- 2/2017 - 10/10/2017
--
-- Nome: José Gerson Fialho Neto
--

-- 1) (2.0) Escreva uma funcao locate, que recebe como entrada um elemento e uma lista de elementos, e retorna a localização (o índice) daquele elemento dentro da lista. 
-- A primeira posição na lista tem índice 0 (zero).
-- Caso o elemento não pertença à lista, deve ser retornado o valor (-1).
-- Exemplos: locate 'x' "abcdewxyz" ------>  6
--           locate 5   [5,98,7,32] ------>  0
--           locate True [False, False] --> -1

locate :: Eq t => t -> [t] -> Int
locate a list | not (elem a list) = -1
locate a list = locateAux a (zip list [0..])

locateAux :: Eq t => t -> [(t, Int)] -> Int
locateAux a ((x,y):xys) | (a == x) = y
                        | otherwise = locateAux a xys

-- 2) (3.0) Escreva uma função que verifique se uma lista está contida em outra (por exemplo, se uma String é substring de outra).
-- Exemplos: substr "abc" "xyz12abrt" ----> False
--           substr "abc" "aaabrsabcfr" --> True
--           substr "aab" "aacrtxxeaayb" -> False

substr :: String -> String -> Bool
substr s1 s2 = elem True [ (s1 == (take (length s1) (drop i s2))) | i <- [1..(length s2)]]

-- 3) Um robô é controlado por 4 comandos: 
--    Left, para girar sua direção à esquerda 90 graus;
--    Right, para girar sua direção à direita em 90 graus;
--    Forward seguido de um número N, que indica um avanço de N metros.
--    Backward seguido de um número N, que indica um retrocesso de N metros.

-- Supondo que o robô comece na posição (0,0) (coordenadas) e direcionado para norte (i.e. para o posição (0,1)): 
-- (3.0) faça uma função destination que informe a localização do robô após uma sequêcia de comandos.

-- Exemplo de posições/coordenadas:
-- (-2, 2) (-1, 2) (0, 2) (1, 2) (2, 2)
-- (-2, 1) (-1, 1) (0, 1) (1, 1) (2, 1)
-- (-2, 0) (-1, 0) (0, 0) (1, 0) (2, 0)
-- (-2,-1) (-1,-1) (0,-1) (1,-1) (2,-1)
-- (-2,-2) (-1,-2) (0,-2) (1,-2) (2,-2)

data Command = Forward Int | Backward Int | TurnLeft |  TurnRight 
               deriving (Eq, Show)
data Direction = North | South | West | East
               deriving (Eq, Show)

-- exemplo: destination (0,0) [Forward 2, TurnLeft, TurnLeft, Forward 1] ---> (0,1)
--          destination (0,0) [Backward 2, Forward 1] ---> (0,-1)

-- destination :: (Int,Int) -> [Command] -> (Int,Int)


destinationAux :: (Int, Int) -> Direction -> Command -> (Int, Int)
destinationAux :: p dir [] = p
destinationAux (x,y) dir (TurnLeft:comms) = destinationAux (direction dir TurnLeft) comms 

-- 4) (2.0) faça uma função faces que informe para qual direção o robô estará voltado ao final de uma sequência de comandos (North, South, East ou West), assumindo que ele começa voltado para a direção North.
-- exemplo: faces North [Forward 2, TurnLeft, TurnLeft, Forward 1] ---> South
--          faces North [Backward 2, Forward 1] ---> North
--          faces North [TurnLeft, TurnLeft, TurnLeft] ---> East

faces ::  Direction -> [Command] -> Direction
faces dir commList = foldl direction dir commList

direction :: Direction -> Command -> Direction
direction dir comm | dir == North && comm == TurnLeft = West
                   | dir == North && comm == TurnRight = East
                   | dir == South && comm == TurnLeft = East
                   | dir == South && comm == TurnRight = West
                   | dir == East && comm == TurnLeft = North
                   | dir == East && comm == TurnRight = South
                   | dir == West && comm == TurnLeft = South
                   | dir == West && comm == TurnRight = North
                   | otherwise = dir
