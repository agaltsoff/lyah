-- Apply twice
twiply :: (a -> a) -> a -> a
twiply f x = f (f x)

-- Apply n times
nply :: (Num n, Eq n) => n -> (a -> a) -> a -> a
nply 0 _ x = x
nply n f x = f (nply (n -1) f x)

-- Test
nply_test = nply 3 (++ "!") (nply 5 (++ "-HA") "A")

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- Collatz

collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz n 
    | even n = n : collatz (div n 2)
    | odd n = n : collatz (n*3 + 1)

numLongCollatz n =  length [head x | x <- map collatz [1..n], length x > 15] 