-- 1
ehTriangulo a b c = if a + b > c &&  b + c > a && a + c > b then True else False

-- 2
tipoTriangulo d e f = if d == e && d == f && e == f then "equilatero" else if d /= e && d /= f && e /= f then "escaleno" else "isosceles"

--3
triangulo g h i = if ehTriangulo g h i == True then tipoTriangulo g h i else "nao eh um triangulo"

--4
somaPares 0 = 0
somaPares n = if rem n 2 == 0 then n + somaPares (n - 2)  else somaPares (n - 1)

--5
somaPot2m m 0 = m
somaPot2m m n = (2 ^ n) * m + somaPot2m m (n - 1)

-- 6
primo n = primo' n (n - 1)
primo' n 1 = True
primo' n d = if rem n d == 0 then False else primo' n (d - 1)

-- 7
seriePI n = seriePI' n 1 1
seriePI' n d s = if d <= n then (s * (4/d)) + seriePI' n (d+2) (negate s) else 0