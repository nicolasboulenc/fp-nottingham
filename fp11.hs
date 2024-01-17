-- 1 Name the function
-- 2 Write down its type
-- 3 Enumerate the cases
-- 4 Define the simple cases
-- 5 List the "ingredients"
-- 6 Define the other cases
-- 7 Think about the result

drop0 :: Int -> [a] -> [a]
drop0 _ [] = []
drop0 0 (x:xs) = x:xs
drop0 n (_:xs) = drop (n-1) xs

dropLast :: [a] -> [a]
dropLast []     = []
dropLast [a]    = []
dropLast (x:xs) = x:dropLast xs