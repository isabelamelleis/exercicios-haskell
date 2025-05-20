-- 1
nprimeiros n [] = []
nprimeiros 0 _ = []
nprimeiros n (x:xs) = x:nprimeiros (n-1) xs

-- 2
ultimo [x] = x
ultimo (x:xs) = ultimo xs

-- 3 
replicar2 0 _ = []
replicar2 n x = x:replicar2 (n-1) x

replicar _ [] = []
replicar n (x:xs) = replicar2 n x ++ replicar n xs

-- 4
fatiar _ _ [] = [] 
fatiar n m (x:xs) = if n > 0 then fatiar (n-1) (m-1) xs else if m >= 0 then x:fatiar n (m-1) xs else [] 

-- 5
menores [] = []
menores ((x,y):xs) = if x < y then (x,y):menores xs else menores xs