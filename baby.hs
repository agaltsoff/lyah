
bmiTell :: (RealFloat a) => a -> a-> String
bmiTell weight height = 
    let
        bmi = weight / height ^ 2
        (skinny, normal, fat) = (18.5, 25.0, 30.0)
    in
        if bmi < skinny then "skinny"
        else if bmi < normal then "normal"
        else if bmi < fat then "fat"
        else "whale"
        
fibo :: (Integral a) => a -> a
fibo 0 = 0
fibo 1 = 1
fibo n = n + fibo (n - 1)

replicate' :: (Integral b) => a -> b -> [a]
replicate' x 0 = []
replicate' x n = x : replicate' x (n - 1)

reverce' :: [a] -> [a]
reverce' [] = []
reverce' (x:xs) = reverce' xs ++ [x]

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort (filter (<x) xs) ++ [x] ++ qsort (filter (>=x) xs)

collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz x = x : collatz (if even x then div x 2 else x * 3 + 1)


-- http://learnyouahaskell.com/higher-order-functions
-- Function application with $