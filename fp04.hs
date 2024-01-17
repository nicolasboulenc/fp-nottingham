
double x = x + x

quadruple x = double (double x)

n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]

-- return last element
last0 :: [a] -> a
last0 [x] = x
last0 (x:xs) = last0 xs

last1 :: [a] -> a
last1 x = head (drop (length x - 1) x) 

last2 :: [a] -> a
last2 x = x !! (length x - 1) 

-- remove last element
init0 :: [a] -> [a]
init0 x = take (length x - 1) x

init1 :: [a] -> [a]
init1 [x] = []
init1 (x:xs) = x:init1 xs