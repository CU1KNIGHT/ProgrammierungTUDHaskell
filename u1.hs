
sum3 :: Int -> Int -> Int -> Int
sum3 x y z = x + y + z

fac:: Int -> Int 
fac 0=1
fac 1=1
fac n=n* fac(n-1)

sumFacs :: Int -> Int -> Int 
sumFacs n m
 | n>m=0
 | otherwise= fac m +sumFacs n (m-1)

fib :: Int -> Int
fib 0=0
fib 1=1
fib n= fib(n-1)+fib(n-2)
