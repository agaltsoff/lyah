
fibo :: (Integral a) => a -> a
fibo 0 = 0
fibo 1 = 1
fibo x = fibo (x - 1) + fibo (x - 2)

max' :: (Ord a) => [a] -> a
max' [] = error "Maximum of an empty list"
max' [x] = x
max' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = max' xs

max'' :: (Ord a) => [a] -> a
max'' [] = error "Maximum of an empty list"
max'' [x] = x
max'' (x:xs) = max x (max'' xs)

replicate' :: (Num n, Ord n) => a -> n -> [a]
replicate' x 0 = []
replicate' x n = x:replicate' x (n - 1)

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort [x] = [x]
qsort (x:xs)
    | x <= minimum xs = x:qsort xs
    | otherwise = qsort (xs ++ x:[])
    
qsort2 :: (Ord a) => [a] -> [a]
qsort2 [] = []
qsort2 (xs) = [ a | a <- xs, a == minimum xs ] ++ qsort2 [ a | a <- xs, a /= minimum xs ]

    
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted
    

isDigit :: Char -> Bool
isDigit = (`elem` ['0'..'9'])

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

elem' :: (Eq a) => a -> [a] -> Bool
elem' y xs = foldl (\res x -> res || (x == y)) False xs
