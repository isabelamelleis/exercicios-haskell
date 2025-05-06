-- 1
pertence n [] = False
pertence n (x:xs) = if n == x then True else pertence n xs

-- 2
intercessao [] ys = []
intercessao (x:xs) ys = if pertence x ys then x:intercessao xs ys else intercessao xs ys

-- 3
inverso [] = []
inverso (x:xs) =  inverso xs ++ [x]

-- 4
nPrimeiros _ [] = []
nPrimeiros 0 _ = []
nPrimeiros n (x:xs) = x:nPrimeiros (n-1) xs
nUltimos n (y:ys) = inverso (nPrimeiros n (inverso(y:ys)))

-- 5
soma2 [] _ = []
soma2 _ [] = []
soma2 (x:xs) (y:ys) = x+y:soma2 xs ys

-- 6
pot2Decrescente 1 = [2]
pot2Decrescente n = 2^n:pot2Decrescente (n-1)
pot2 n = inverso (pot2Decrescente n)

-- 7
intercalacao [] ys = ys
intercalacao xs [] = xs
intercalacao (x:xs) (y:ys) = if x <= y then x:intercalacao xs (y:ys) else y:intercalacao (x:xs) ys

-- 8
menor [x] = x
menor (x:xs) = if x < menor xs then x else menor xs

-- 9
removerElem n [] = []
removerElem n (x:xs) = if n == x then xs else x:removerElem n xs

-- 10
ordenar [] = []
ordenar xs = (menor xs):(ordenar(removerElem (menor xs) xs))

-- 11
ins n [] = [n]
ins n (x:xs) = if pertence n (x:xs) == True then ordenar (x:xs) else ordenar (n:(x:xs))

-- 12
enesimo 1 (x:xs) = x
enesimo n (x:xs) = enesimo (n-1) xs

-- 13
repetir 1 e = [e]
repetir n e = e:repetir (n-1) e

-- 14
numString 0 = []
numString n = numString (div n 10) ++ [int2Char (rem n 10)]
int2Char :: Int -> Char
int2Char d = toEnum (d+48)
-- toEnum transforma um número inteiro em seu caracter correspondente em relação à tabela ASCII

-- 15
stringNum :: String -> Int
stringNum [] = 0
stringNum (x:xs) = char2Int x * (10 ^ length xs) + stringNum xs
char2Int :: Char -> Int
char2Int d = fromEnum d - 48
-- fromEnum é o oposto de toEnum, pois transforma um caracter em seu número correspondente

-- 16
bin2int [] = 0
bin2int (x:xs) = (char2Int x) * 2 ^ (length xs) + bin2int xs

-- 17
int2bin 0 = []
int2bin n = int2bin (div n 2) ++ [int2Char (rem n 2)]

-- 18
minusculas [] = []
minusculas (x:xs) = if fromEnum x < 97 then toEnum (fromEnum x + 32):minusculas xs else x:minusculas xs