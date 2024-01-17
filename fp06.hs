
safetail0 :: [a] -> [a]
safetail0 xs = if null xs then []
    else tail xs

safetail1 :: [a] -> [a]
safetail1 xs
    | null xs = []
    | otherwise = tail xs

safetail2 :: [a] -> [a]
safetail2 [] = []
safetail2 (_:xs) = xs


(||) :: Bool -> Bool -> Bool
True || _ = True
False || False = False

and :: Bool -> Bool -> Bool 
and a b =   if a == True then
                if b == True then True
                else False
            else False 


and2 :: Bool -> Bool -> Bool 
and2 a b =  if a == True then b
            else False
