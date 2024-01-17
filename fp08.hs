
-- evaluate all logical values in list
and' :: [Bool] -> Bool
and' []          = True
and' (False:xs)  = False
and' (x:xs)      = x && and' xs

-- concanate a list of lists
concat' :: [[a]] -> [a]
concat' []       = []
concat' (xs:xss)   = xs ++ concat' xss


-- produce n indentical elements in list
replicate' :: Int -> a -> [a]
replicate' 0 _   = []
replicate' n x   = x : (replicate' (n-1) x)


-- evalute to index in list
(!!) :: [a] -> Int -> a
(!!) (x:_) 0   = x
(!!) (_:xs) n   = (Main.!!) xs (n-1)


-- evaluate true if value is in list
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' y (x:xs) = if y == x  then True 
                            else elem' y xs

-- insert 
insert :: Int -> [Int] -> [Int]
insert a [] = [a]
insert a (x:xs) = if x < a  then x : insert a xs 
                            else a : x : xs


-- pass 2 sorted lists return 1 merged sorted list
merge' :: Ord a => [a] -> [a] -> [a]
merge' xs [] = xs
merge' [] ys = ys
merge' (x:xs) (y:ys) = if x < y then x : merge' xs (y:ys)  
                                else y : merge' (x:xs) ys
