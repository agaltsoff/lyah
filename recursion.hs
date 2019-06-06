
fibo :: Integer -> [Integer]

fibo 0 = []
fibo 1 = [0]
fibo 2 = [0, 1]
fibo n = reverse ((sum fibo') : reverse fibo') 
    where 
        fibo' = fibo (n - 1)
        

max' :: (Ord n) => [n] -> n

max' [] = error "max' on emptyist"
max' [x] = x
max' (x:xs) = if x > max'' then x else max''
    where
        max'' = max' xs