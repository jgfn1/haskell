-- Primeira Prova de Paradigmas de Linguagens Computacionais
-- 2/2016 - 29/09/2016
--
-- Nome:
--
{- O formato CSV (comma-spearated value) é bastante utilizado para
disponibilizar dados abertos ou para disponiblizar arquivos com
logs.
Abaixo temos um exemplo de um arquivo assim, onde a primeira
linha é o título de cada coluna e as demais são os valores de cada
coluna separados por vírgulas.
exemplo de arquivo de log:
-------------------------------------
Day;Time;Event;Card;
2016-09-27;19:31:52;Normal;208772;
2016-09-27;18:12:02;Normal;155759;
2016-09-26;17:12:02;Normal;155759;
2016-09-26;16:11:02;Denied;188758;
2016-09-25;19:12:02;Normal;155759;
-------------------------------------
Neste exemplo, de um log de controle de acesso de determinado mês,
temos:
um campo de data (ano-mês-dia) e hora (hora-minuto-segundo),
um campo com o tipo de evento de acesso (Normal ou Denied),
e o número de identificação do usuário.
Nesse exemplo temos 5 eventos, em 3 dias (25, 26 e 27 de
setembro), com 3 usuários diferentes.
Abaixo temos uma String a ser usada para testes, sem a linha de
cabeçalho mas com 5 linhas: -}

logSetembro = "2016-09-27;19:31:52;Normal;208772;\n2016-09-27;18:12:02;Normal;155759;\n2016-09-26;17:12:02;Normal;155759;\n2016-09-26;16:11:02;Denied;188758;\n2016-09-25;19:12:02;Normal;155759;"

{- Considerando que o arquivo está em uma String, e que os dados são
de um mesmo mês, você deve escrever funções que dêem as seguintes
informações: -}
-- 1) (2.5) Quantos acessos foram permitidos e quantos foram negados no período? (isto é, na string toda).
-- o resultado é uma tupla, onde o primeiro valor é o total de acessos permitidos (Normal) e o segundo o de Negados (Denied)
-- exemplo:
-- tiposDeAcesso logSetembro -----> (4,1)
tiposDeAcesso :: String -> (Int, Int)
tiposDeAcesso [] = (0,0)
tiposDeAcesso str = ((countOccur (eventExtract str) "Normal"), (countOccur (eventExtract str) "Denied"))

eventExtract :: String -> [String]
eventExtract [] = []
eventExtract str = [(take 6 (drop 20 str))] ++ eventExtract (drop 35 str)

countOccur :: Eq t => [t] -> t -> Int
countOccur [] _ = 0
countOccur list x = length [y | y <- list, y == x] 

-- 2) (2.5) Quantas tentativas de acessos ocorreram em cada dia?
-- (considere que o arquivo já está ordenado por data e hora) cada dia
-- com acesso deve aparecer em uma lista de tuplas, o primeiro valor da
-- tupla é o dia do mês e o segundo o número de acessos.
-- exemplo:
-- acessosPorDia logSetembro -----> [(27,2),(26,2),(25,1)]
-- 2 acessos dia 27, 2 dia 26 e 1 dia 25
remove :: Eq t => t -> [t] -> [t]
remove _ [] = []
remove y l = [x | x <- l, x /= y]

acessosPorDia :: String -> [(Int, Int)]
acessosPorDia [] = []
acessosPorDia str = acessosPorDiaAux (dayExtract str) []

acessosPorDiaAux :: [String] -> [(Int, Int)] -> [(Int, Int)]
acessosPorDiaAux [] _ = []
acessosPorDiaAux (d:ds) partialResult = [((read d), (countOccur (d:ds) d))] 
                                      ++ acessosPorDiaAux (remove d ds) ([((read d), (countOccur (d:ds) d))] ++ partialResult)

dayExtract :: String -> [String]
dayExtract [] = []
dayExtract str = [(take 2 (drop 8 str))] ++ dayExtract (drop 35 str)

-- 3) (2.5) Quantos acessos cada usuário realizou no período?
-- exemplo:
-- acessosPorUsuario logSetembro -----> [(208772,1),(155759,3),(188758,1)] 
-- 3 acessos do usuário 155759 e
-- 1 acesso dos outros

acessosPorUsuario :: String -> [(Int, Int)]
acessosPorUsuario [] = []
acessosPorUsuario str = acessosPorUsuarioAux (userExtract str) []

acessosPorUsuarioAux :: [String] -> [(Int, Int)] -> [(Int, Int)]
acessosPorUsuarioAux [] _ = []
acessosPorUsuarioAux (d:ds) partialResult = [((read d), (countOccur (d:ds) d))] 
                                      ++ acessosPorDiaAux (remove d ds) ([((read d), (countOccur (d:ds) d))] ++ partialResult)

userExtract :: String -> [String]
userExtract [] = []
userExtract str = [(take 6 (drop 27 str))] ++ userExtract (drop 35 str)

-- 4) (2.5) escreva a função converte, que transforma os dados
-- armazenados na String para o seguinte tipo de dados:
type Dia = String
type Hora = String
type Usuario = String
data LogEntry = Permitido Dia Hora Usuario
              | Negado Dia Hora Usuario
              deriving (Show, Eq)
-- exemplo:
-- converte logSetembro -----> [Permitido "2016-09-27" "19:31:52" "208772",Permitido "2016-09-27" "18:12:02" "155759",Permitido "2016-09-26" "17:12:02" "155759",Negado "2016-09-26" "16:11:02" "188758",Permitido "2016-09-25" "19:12:02" "155759"]
-- função auxiliar para as questões
-- strToInt :: String -> Int
-- strToInt str = read str

converte :: String -> [LogEntry]
converte [] = []
converte str = converteAux . extractAll str

converteAux :: [String] -> [LogEntry]
converteAux [] = []
converteAux ("Normal":b:c:d:abcds) = Permitido b c d:converteAux abcds
converteAux ("Denied":b:c:d:abcds) = Negado b c d:converteAux abcds

extractAll :: String -> [String]
extractAll [] = []
extractAll str = [(take 6 (drop 20 str))] ++ [(take 10 str)] ++ [(take 8 (drop 11 str))] ++ [(take 6 (drop 27 str))] ++ extractAll (drop 35 str)

hourExtract :: String -> [String]
hourExtract [] = []
hourExtract str = [(take 8 (drop 11 str))] ++ hourExtract (drop 35 str)