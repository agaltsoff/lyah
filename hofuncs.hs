-- Apply twice
twiply :: (a -> a) -> a -> a
twiply f x = f (f x)

-- Apply n times
nply :: (Num n, Eq n) => n -> (a -> a) -> a -> a
nply 0 _ x = x
nply n f x = f (nply (n -1) f x)