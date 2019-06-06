
fibo :: Integer -> [Integer]
fibo 0 = []
fibo 1 = [0]
fibo 2 = [0, 1]
fibo n = reverse ((sum fibo') : reverse fibo') 
    where 
        fibo' = fibo (n - 1)