-- 2) Um sorteio da Mega-Sena pode ser representado por uma lista de seis números.
-- Um conjunto de cartões de apostas pode ser representado por uma lista
-- de listas (cada lista representando um cartão).
-- Assuma que os número do resultado premiado/sorteado e os números em cada cartão
-- estão ordenados.
-- Defina funções para:
-- (1.0 ponto) a função premiados retorna o numero de cartões premiados com a sena.
type Resultado = [Int]
type Jogos = [[Int]]
 
premiados :: Resultado -> Jogos -> Int
premiados [] _ = 0
premiados _ [] = 0
premiados  result games = (length [x | x <- games, x == result])

-- (1.0 ponto) a função acertos retorna a lista com o número de acertos em cada
-- cartão (do primeiro cartão, do segundo, do terceiro, etc.).
acertos :: Resultado -> Jogos -> [Int]
acertos result games = [acertosAux result x | x <- games]

acertosAux :: Resultado -> [Int] -> Int
acertosAux [] _ = 0
acertosAux _ [] = 0
acertosAux result game = length [x | x <- result, elem x game]

-- (1.0 ponto) a função numPremios retorna uma tupla de três inteiros contendo a
-- quantidade de cartões premiados com 4, 5 ou 6 acertos, respectivamente.
numPremios :: Resultado -> Jogos -> (Int, Int, Int)
numPremios result games = let acertosList = acertos result games in (length [x | x <- acertosList, x == 4], length [x | x <- acertosList, x == 5], length [x | x <- acertosList, x == 6])

-- 3) Uma linguagem de programação baseada em pilha possui apenas uma pilha (stack)
-- onde ficam os dados/operandos e todas as instruções são apenas de empilhar,
-- desempilhar ou fazer operações consumindo (lendo) os dados no topo da pilha e
-- deixando o resultado final o topo da pilha.
-- Dados os tipos de dados abaixo e os exemplos, escreva um interpretador que
-- executa as instruções com o comportamento abaixo:
data Instrucao = PUSH Int | POP | ADD | SUB | DUP
               deriving Show
type Pilha = [Int]

-- (1.0 ponto) evalI avalia uma única instrução em uma dada pilha
-- exemplos: eval ADD [1,2,3,4,5] ---> [3,3,4,5] (soma 1+2)
-- exemplos: eval DUP [5,1] ---> [5,5,1] (repete/copia o valor no topo da pilha)
-- exemplos: eval SUB [1,2,3,4] ---> [-1,3,4] -- calcula 1-2=-1
-- exemplos: eval ADD [1,2,3,4,5] ---> [3,3,4,5] (soma 1+2)
-- exemplos: eval PUSH 7 [1,2,3] ---> [7,1,2,3] insere o 7 no topo
-- exemplos: eval POP [8,2,3] ---> [2,3] -- remove 8

eval :: Instrucao -> Pilha -> Pilha
eval ADD (a:b:abs) = (a + b:abs)
eval DUP (a:as) = (a:a:as)
eval SUB (a:b:abs) = (a - b:abs)
eval (PUSH x) (a:as) = (x:a:as)
eval (PUSH x) [] = [x]
eval POP (a:as) = as

-- (1.0 ponto) evalProg avalia um programa (sequência de instruções) a partir de
-- uma pilha inicial vazia e retorna o estado final da pilha depois da avaliação
-- exemplos: evalProg [PUSH 3, PUSH 5, DUP, ADD, SUB] ---> [7] (5+5-3)
evalProg :: [Instrucao] -> Pilha
evalProg [] = []
evalProg instr = evalProgAux instr []

evalProgAux :: [Instrucao] -> Pilha -> Pilha
evalProgAux [] stack = stack
evalProgAux (i:is) stack = evalProgAux is (eval i stack)

-- 4) (2.0 pontos) faça uma função translate que traduz expressões (tipo Expr,
-- abaixo) para uma sequência (lista) de instruções que, se usadas com o avaliador
-- da questão 4, avaliam a expressão.
-- Exemplo:
-- translate (Soma (Literal 5) (Dobra (Subtrai (Literal 4) (Literal 1)))) ----> [PUSH 1, PUSH 4, SUB, DUP, ADD, PUSH 5, ADD]

data Expr = Literal Int -- um número
          | Soma Expr Expr -- soma as duas expressões
          | Subtrai Expr Expr -- subtrai a segunda expressão da primeira
          | Dobra Expr
          deriving (Show)

translate :: Expr -> [Instrucao]
translate expr = translateAux expr []

translateAux :: Expr -> [Instrucao] -> [Instrucao] 
translateAux (Literal x) [] = [PUSH x]
translateAux (Dobra expr1) [] = [DUP]
translateAux (Soma expr1 expr2) [] = translateAux expr2 (translateAux expr1 [ADD])
translateAux (Subtrai expr1 expr2) [] = translateAux expr2 (translateAux expr1 [SUB])
translateAux (Literal x) list = (PUSH x):list
translateAux (Dobra expr1) list = (translateAux expr1 [DUP]) ++ list
translateAux (Soma expr1 expr2) list = (translateAux expr2 (translateAux expr1 [ADD])) ++ list
translateAux (Subtrai expr1 expr2) list = (translateAux expr2 (translateAux expr1 [SUB])) ++ list

-- 5) (1.0 ponto) qual a diferença entre avaliação estrita e preguiçosa (lazy)?
-- Mostre exemplos desta diferença.
{-
A avaliação preguiçosa só calcula o valor de uma expressão se for realmente necessário, enquanto
a avalição estrita calcula tudo independentemente da necessidade.
Ex:.
	zip [1, 2, 3] [0..] em Haskell funciona pois ele só usa os elementos da lista infinita [0..] que são
	necessários, ignorando os infinitos elementos restantes; enquanto em uma linguagem com avaliação estrita
	como C o programa calcularia os elementos de [0..] indefinidamente.
-}