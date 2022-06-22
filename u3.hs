-- Aufgabe 1
--a 
isPrefix:: String -> String -> Bool
isPrefix "" _= True
isPrefix _ ""=False
isPrefix (x:xs) (y:ys)
 |x==y =True && isPrefix xs ys
 |otherwise =False
 
countPattern:: String -> String -> Int
countPattern "" x  = length x +1
countPattern x ""= 0
countPattern xs ys
 |isPrefix xs ys= 1 + countPattern xs (tail ys)
 |otherwise = countPattern xs (tail ys)
 
data BinTree = Branch Int BinTree BinTree | Nil deriving Show
--a
myTree::BinTree
myTree=Branch 0 Nil (Branch 3 (Branch 1 Nil Nil) (Branch 5 Nil Nil))

--b

isIdent:: BinTree -> BinTree -> Bool
isIdent Nil Nil=True
isIdent (Branch x l r) Nil=False
isIdent  Nil(Branch x l r) =False
isIdent (Branch y l1 r1 ) (Branch x l2 r2)= x==y && isIdent l1 l2 && isIdent r1 r2 

--c 
insert :: BinTree -> [Int] -> BinTree

-- lösung von eric Kunze

insert t     [] = t
insert t (n:ns) = insert t' ns
  where
    t' = insertSingle t n
    insertSingle Nil     
     n = Branch n Nil Nil
    insertSingle (Branch x l r) n
      | n < x     = Branch x (insertSingle l n) r
      | otherwise = Branch x l (insertSingle r n)


--D
unwind :: BinTree -> [Int]
unwind Nil =[]
unwind (Branch x l r)= [x] ++ unwind l ++ unwind r
