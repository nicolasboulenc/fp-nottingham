
filter0 :: (a -> Bool) -> [a] -> [a]
filter0 f []        = []
filter0 f (x:xs)    = if f x    then x : filter0 f xs
                                else filter0 f xs

-- try to use guards where possible
filter1 :: (a -> Bool) -> [a] -> [a]
filter1 f [] = []
filter1 f (x:xs)
    | f x       = x : filter1 f xs
    | otherwise = filter1 f xs

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 f xs = [ x | x <- xs, f x]


-- [f x | x <- xs, p x]
f0 xs p f = map f (filter p xs)

