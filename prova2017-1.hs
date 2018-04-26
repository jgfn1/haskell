-- Primeira Prova de Paradigmas de Linguagens Computacionais 
-- 1/2017 - 01/05/2017
--
-- Nome: 
--
{- 1) (2.5) Escreva uma função que verifica se uma lista já está ordenada, 
   do menor para o maior elemento..
   exemplo: isSorted [1,6,8,9,9] ------> True
            isSorted [1,6,8,7,9] ------> False
   Dica: verifique se sua resposta funciona para listas de tamanho ímpar.
-}
isSorted :: Ord t => [t] -> Bool
isSorted [] = True
isSorted (a:[]) = True
isSorted (a:b:abs)  | a <= b = isSorted (b:abs)
                    | otherwise = False

{- 2) (2.5) O método de ordenação bubble-sort funciona da seguinte forma: 
   cada elemento da lista de entrada é comparado com o seguinte, 
   e se eles não estiverem em ordem (do menor para o maior) sua posição na lista resultante é trocada,
   e a comparação continua com a nova ordem.Esse processo é repetido até que a lista esteja ordenada 
   (nenhuma troca seja mais necessária).
   exemplo, passo a passo: 
       bSort [4,8,3,6,1,8] ----> compara 4 e 8, 8 e 3 (troca, pois 8 > 3), 8 e 6(troca novamente), 8 e 1 (troca novamente) e 8 e 8  
                                   ----> [4,3,6,1,8,8]
       repetindo o processo, temos  ---> [3,4,1,6,8,8] ---> [3,1,4,6,8,8]  ---> [1,3,4,6,8,8]
Implemente a função bSort.
Dica 1: use funções auxiliares, que façam parte do processo;
Dica 2: verifique que sua solução funciona para listas de tamanho ímpar.
-}
bSort :: Ord t => [t] -> [t]
bSort [] = []
bSort (a:[]) = [a]
bSort (a:b:abs) | (not (isSorted (a:b:abs))) &&  (a > b) = bSort (b:a:abs)
                | (not (isSorted (a:b:abs))) &&  (a <= b) = bSort (a:(bSort (b:abs)))
                | otherwise = (a:b:abs)

{- 3) (2.5) explique como funciona e informe qual o resultado da execução das 
   seguintes expressões. Caso estejam erradas explique por que.
a) map (\x -> x + x) [3,5,7,9]
	A função map aplica uma determinada função a cada elemento de uma lista, no caso, ela está somando cada elemento da lista a ele mesmo e o resultado
	será [6, 10, 14, 18].

b) filter (\x -> x < 7) [5,7,9,11]
	A função filter refaz uma nova lista com a condiçao estabelecida que foi passada como argumento, nesse caso ela recriará a lista apenas com os elementos
	menos que 7, logo o resultado será [5].

c) foldr1 (*) [-2,0,2,4]
	A função foldr1 combina todos os elementos de uma lista começando pelo último de acordo com uma função passada como parâmetro,
	nesse caso, ela irá multiplicar todos os elementos da lista, portanto o resultado será o inteiro 0.

d) foldr (+) 20 [-2,0,2,4]
	A função foldr combina todos os elementos de uma lista começando pela combinação do argumento com o último elemento de acordo com uma função passada 
	como parâmetro, nesse caso, ela irá somar todos os elementos da lista entre si e com o argumento, portanto o resultado será o inteiro 24.

e) (map (+2) . filter (<7)) [5,7,9,11]
	A composição das funções map e filter é equivalente a aplicar a função filter primeiramente e então aplicar a map, portanto o resultado parcial será
	[5] e o final será [7].
-}

{- 4) (2.5) Dada o tipo de dados Tree t, abaixo, que reresenta uma árvore binária 
com informações (valores) em seus nós, faça uma função isSortedTree que informa se uma árvore está ordenada, ou seja, os valores em nós ou folhas na sub-àrvore à
esquerda são sempre menores ou iguais ao valor do nó, e os da sub-árvore à direita sempre maiores ou iguais.
-}

-- isSortedTree testeOrdenado ----> True
-- isSortedTree testeNaoOrdenado ----> False

data Tree t = Node t (Tree t) (Tree t) | Leaf t
            deriving (Ord, Eq)

testeOrdenado :: Tree Int
testeOrdenado = Node 10 (Node 5 (Leaf 3) (Leaf 6)) (Node 15 (Leaf 14) (Leaf 17))

testeNaoOrdenado :: Tree Int
testeNaoOrdenado = Node 10 (Node 5 (Leaf 3) (Leaf 6)) (Node 15 (Leaf 16) (Leaf 17))

isSortedTree :: Ord t => Tree t -> Bool
isSortedTree (Leaf x) = True
isSortedTree (Node x (Leaf left) (Leaf right))         | (left <= x) && (right >= x) = (isSortedTree (Leaf left)) 
                                                                                    && (isSortedTree (Leaf right))
                                                       | otherwise = False
isSortedTree (Node x (Node left a b) (Node right c d)) | (left <= x) && (right >= x) = (isSortedTree (Node left a b)) 
                                                                                    && (isSortedTree (Node right c d)) 
                                                       | otherwise = False