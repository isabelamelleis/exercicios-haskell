-- lines :: String → [String] -- Divide um texto em linhas
-- words :: String → [String] -- Divide uma linha em palavras



-- o primeiro parâmetro é uma condição e o segundo uma lista. A função filter retorna uma lista com os elementos da lista inicial que se encaixam na condição dada no primeiro parâmetro
filter' _ [] = []
filter' p (x:xs) =  if p x then x:filter' p xs else filter' p xs
-- coloque no terminal o ex: filter' (\x -> x < 3) [1, 2, 3, 4, 5]
-- coloque no terminal o ex: filter' (>2) [1, 2, 3, 4, 5]
-- coloque no terminal o ex: filter' (\x -> length x > 4) ["aaaa","bbbbbbbbbbbbb","cc"]



-- pega o último item da lista e faz a operação (indicada pelo primeiro parâmetro) com o segundo parâmetro. Após isso, faz a próxima operação com o resultado da primeira e o penúltimo item da lista, e assim por diante.
foldr' f n [] = n
foldr' f n (x:xs) = f x (foldr' f n xs)
-- coloque no terminal o ex: foldr' (+) 5 [1,2,3,4]
-- coloque no terminal o ex: foldr' (&&) True [1>2,3>2,5==5]
foldl' _ n [] = n
foldl' f n (x:xs) = foldl' f (f n x) xs



-- recebe duas listas e cria uma tupla com os elementos de ambas as listas que estejam nas mesmas posições
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys



-- recebe uma lista e uma condição. Se todos os elementos da lista satisfazerem a condição, a função retorna True, senão, retorna False
all' _ [] = True
all' n (x:xs) = n x && all' n xs



-- recebe uma lista e uma condição e faz uma lista com os elementos da lista inicial modificados de acordo com a condição
map' n [] = []
map' n (x:xs) = (n x):map' n xs



-- recebe dois parâmetros e verifica se são iguais (EQual), ou se o primeiro é menor que o segundo (Less Than) ou então se o primeiro é maior que o segundo (Greater Than)
compare' x y = if x == y then EQ else if x < y then LT else GT



-- recebe um valor n e verifica se este valor pertence a lista
elem' n [] = False
elem' n (x:xs) = if n == x then True else elem' n xs