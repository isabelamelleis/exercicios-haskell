import Data.List (sortBy, groupBy)

type Doc = String
type Line = String
type Word' = String



-- obs: getLine é o "scanf" de haskell
main = do putStr "Digite o nome do arquivo: \n\n"
          arq <- getLine
          txt <- readFile arq
          putStr (makeindexFormatado (makeindex txt))

makeindex txt = shorten (almalgamate (sortLs (allNumWords (numLines (lines txt)))))
makeindexFormatado xs = unlines (map (\(n, m) -> m ++ " - " ++ show n) xs)
-- o map está pegando cada dupla da lista xs e criando uma lista com elas transformadas em string, como: ("Ciencia - [1,2]", "Computacao - [1,2]")
-- a função "show" está transformando a lista de inteiros em string
-- unlines junta todas as strings da lista criada pelo map fazendo em uma só string grande e printa fazendo quebra de linhas



-- a   "lines" já é uma função pronta em haskell



-- b
numLines :: [Line] -> [(Int,Line)]
numLines' _ [] = []
numLines' n (x:xs) = (n, x):numLines' (n+1) xs
numLines xs = numLines' 1 xs
-- saída: [(1,"Departamento de Ciencia da Computacao"),(2,"Curso de Ciencia da Computacao"),(3,"Programacao Funcional")]



-- c
allNumWords [] = []
allNumWords ((n, x):xs) = zip (repeat n) (filter (\y -> length y > 3) (words x)) ++ allNumWords xs
-- saída: [(1,"Departamento"),(1,"Ciencia"),(1,"Computacao"),(2,"Curso"),(2,"Ciencia"),(2,"Computacao"),(3,"Programacao"),(3,"Funcional")]



--d
sortLs [] = []
sortLs x = sortBy (\(_, a) (_, b) -> compare a b) x
-- saída: [(1,"Ciencia"),(2,"Ciencia"),(1,"Computacao"),(2,"Computacao"),(2,"Curso"),(1,"Departamento"),(3,"Funcional"),(3,"Programacao")]



-- e
-- obs: o groupBy faz uma lista de tuplas dentro de uma lista de tuplas: [[(1,"Ciencia"), (2,"Ciencia")], [(1,"Computacao"), (2,"Computacao")]]
-- obs 2: head grp = (1,"Ciencia")
--        snd (head grp) = "Ciencia"
almalgamate xs = map (\x -> (map fst x, snd (head x))) (groupBy (\(_, a) (_, b) -> a == b) xs)
-- saída: [([1,2],"Ciencia"),([1,2],"Computacao"),([2],"Curso"),([1],"Departamento"),([3],"Funcional"),([3],"Programacao")]



-- f
-- obs: elem é uma função que verifica se um elemento x está em uma lista xs, e retorna True se estiver
-- obs2: filter recebe uma lista e retorna uma lista com todos os elementos que obedeçam a condição dada no primeiro parâmetro
shorten :: [([Int], String)] -> [([Int], String)]
shorten xs = map (\(n, m) -> (removerRepetidos n, m)) xs
-- removerRepetidos está recebendo n, e assim que a função termina é agrupado em uma lista com m

removerRepetidos :: Eq a => [a] -> [a]
removerRepetidos [] = []
removerRepetidos (x:xs) = if elem x xs then x:removerRepetidos (filter (/= x) xs) else x:removerRepetidos xs