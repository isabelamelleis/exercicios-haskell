import Data.List (sort)
import Data.Char

-- Conta quantos elementos de uma lista satisfazem uma determinada condição.
quantosSatisfazem n (x:xs) = length (filter n (x:xs))
-- usando lambda: quantosSatisfazem = (\n xs -> length (filter n xs))


-- Verifica se todos os elementos de uma lista são iguais.
todosIguais [x] = True
todosIguais [] = True
todosIguais (x:y:xs) = if x == y then todosIguais xs else False


-- Recebe uma lista de funções e um valor e aplica todas as funções ao valor.
-- ex: aplicarVariacoes [(+1), (*2), (^2)] 3 => [4,6,9]
aplicarVariacoes [] n = []
aplicarVariacoes (x:xs) n = (x n):aplicarVariacoes xs n


-- Dada uma lista e um elemento, insire esse elemento entre cada par de elementos da lista.
intercalar _ [x] = [x]
intercalar n (x:xs) = x:n:intercalar n xs


-- Recebe uma lista de números e retorna uma lista ordenada com apenas os números pares 
paresEmOrdem' [] = []
paresEmOrdem' (x:xs) = if mod x 2 == 0 then x:paresEmOrdem' xs else paresEmOrdem' xs
paresEmOrdem xs = sort (paresEmOrdem' xs)


-- Recebe uma lista de números e retorna a mesma lista só que com os números ímpares duplicados
duplicarImpares [] = []
duplicarImpares (x:xs) = if mod x 2 /= 0 then x:x:duplicarImpares xs else x:duplicarImpares xs


-- Altera o sinal de todos os elementos da lista de números.
alterarSinais xs = map (negate) xs


-- Recebe uma lista de duplas e inverte a ordem dos elementos em cada dupla
compDuplas [] = []
compDuplas ((x,y):xs) = (y,x):compDuplas xs


-- Recebe uma lista e retorna uma lista de duplas, com o primeiro elemento da dupla sendo o índice do segundo elemento da dupla em relação a lista inicial
paresComIndice [] = []
paresComIndice xs = zip [0..] xs


-- Divide a lista em pares (ignora o último elemento caso a lista seja ímpar).
dividirEmDois (x:y:xs) = (x, y) : dividirEmDois xs
dividirEmDois _ = []


-- Recebe duas listas de números desordenadas e retorna uma lista ordenada
intercalarListas' xs [] = xs
intercalarListas' [] ys = ys
intercalarListas' (x:xs) (y:ys) = (x:y:intercalarListas' xs ys)
intercalarListas xs ys = sort (intercalarListas' xs ys)


-- Recebe uma lista e uma condição e retorna os índices dos elementos que se encaixam na condição
indices n xs = map fst (filter (n . snd) (zip [1..] xs))


-- Recebe uma string e retorna sem nenhuma consoante.
semConsoante [] = []
semConsoante (x:xs) = if elem x "AEIOUaeiou" then x:semConsoante xs else semConsoante xs


-- Recebe uma string e retorna apenas as letras maiúsculas
maiusculas [] = []
maiusculas (x:xs) = if isUpper x then x:maiusculas xs else maiusculas xs


-- Recebe uma string e retorna quantas vogais existem.
contaVogais' [] = []
contaVogais' (x:xs) = if elem x "AEIOUaeiou" then x:contaVogais' xs else contaVogais' xs
contaVogais xs = length (contaVogais' xs)


-- Recebe uma lista de números e faz a somatória dos divisores de 3
somaMultiplosDe3' [] = []
somaMultiplosDe3' (x:xs) = if mod x 3 == 0 then x:somaMultiplosDe3' xs else somaMultiplosDe3' xs
somaMultiplosDe3 xs = sum (somaMultiplosDe3' xs)


-- Recebe um número e retorna uma lista com todos os divisores deste número
divisores' n 0 = []
divisores' n x = if mod n x == 0 then x:divisores' n (x-1) else divisores' n (x-1)  
divisores n = reverse (divisores' n n)


-- Recebe uma lista e um elemento e retorna quantas vezes tal elemento aparece na lista
quantElem' n [] = []
quantElem' n (x:xs) = if n == x then x:quantElem' n xs else quantElem' n xs 
quantElem n xs = length (quantElem' n xs)


-- Recebe uma lista e retorna o maior elemento.
maiorElem [x] = x
maiorElem (x:xs) = max x (maiorElem xs)


-- Recebe uma string e elimina todos os espaços que ela possui
removeEspacos [] = []
removeEspacos (x:xs) = if x == ' ' then removeEspacos xs else x:removeEspacos xs


-- Recebe uma lista e dois parâmetros "n" e "m" que indicam os índices de onde deve começar e terminar a nova lista
fatiar n m xs = map snd (filter ((<=m) . fst) (filter ((>=n) . fst) (zip [0..] xs)))


-- Recebe uma lista de strings e retorna a lista sem nenhuma vogal.
ehVogal' x = notElem x "aeiouAEIOU"
semVogal xs = map (filter ehVogal') xs


-- Mesma coisa da anterior, mas feita de outra forma
temVogal [] = []
temVogal (x:xs) = if elem x "AEIOUaeiou" then temVogal xs else x:temVogal xs
semVogal' [] = []
semVogal' (x:xs) = (temVogal x):semVogal' xs