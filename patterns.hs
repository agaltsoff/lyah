
head' :: [a] -> a
head' [] = error "Head of empty list"
head' (x:xs) = x

