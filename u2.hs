fac:: Int -> Int
fac 0=1
fac n= n* fac(n-1)


bincoeff :: Int -> Int -> Int
bincoeff n k
 | n==k = 1
 | k==0 = 1
 | k==1 = n
 | n<k = 0
 | otherwise = fac n `div`((fac k)*(fac (n-k)))
 
--a 
prod :: [Int] -> Int
prod []=1
prod (x:xs)= x* prod xs

--b 
rev :: [Int] -> [Int]
rev []=[]
rev (x:xs) =rev xs ++ [x]

--c
excl :: Int -> [Int] -> [Int]
excl _ []= []
excl y (x:xs)
 | x==y = excl y xs
 |otherwise = [x] ++ excl y xs
 
--d 

isOrd :: [Int] -> Bool
isOrd [] = True
isOrd [x]=True
isOrd (x:y:xs)
 | x <= y= True && isOrd (y:xs)
 |otherwise =False

{-
isOrd :: [Int] -> Bool
isOrd [] = True
isOrd [x]=True
isOrd (x:y:xs)= x <= y && isOrd (y:xs)
-}

--e
merge:: [Int] ->[Int] -> [Int]
merge [] x = x
merge x []=x 
merge (x:xs) (y:ys) 
 |x <= y = [x]++ merge xs (y:ys)
 |otherwise = [y] ++ merge (x:xs) ys

--f
fib:: Int -> Int
fib 0=1
fib 1=1
fib n= fib(n-1)+fib(n-2)

fibs :: [Int]
fibs = fibAppend 0
 where fibAppend x = fib x : fibAppend (x+1)
{-
fibsInternet:: [Integer]
fibsInternet = 1 : 1 : zipWith (+) fibs (tail fibs)
-}

fib' :: Int -> Int
fib' i = stack 1 1 i
    where 
        stack f0 f1 0 = f0
        stack f0 f1 i = stack f1 (f0 + f1) (i-1)


fibs' :: [Int]
fibs' = fib_stack 1 1
    where
      fib_stack x y =  x : fib_stack y (x+y)




