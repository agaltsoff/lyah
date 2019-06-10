
fibo :: Integer -> Integer

fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n - 1) + fibo (n - 2)

fiboo :: Integer -> [Integer]

fiboo 0 = [0]
fiboo 1 = [0, 1]
fiboo n = reverse (( rfiboo' !! 0 + rfiboo' !! 1 ) : rfiboo') 
    where
        rfiboo' = reverse (fiboo (n - 1))

max' :: (Ord n) => [n] -> n

max' [] = error "max' on emptyist"
max' [x] = x
max' (x:xs) = if x > max'' then x else max''
    where
        max'' = max' xs