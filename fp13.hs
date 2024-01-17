
-- the count down problem
-- 6 natural numbers, 4 operators, 1 target

-- [1, 3, 7, 10, 25, 50]
-- +, -, * /
-- 750


data Op = Add

data Expr = Val Int | App Op Expr Expr

apply :: Op -> Int -> Int -> Int
apply _ _ _ = 0

valid :: Op -> Int -> Int -> Bool
valid _ _ _ = True

eval :: Expr -> [Int]
eval _ = []


choiceNum :: Eq a => [a] -> [[a]]
choiceNum [] = []
choiceNum xs = [[a]                 | a<-xs] ++
                [[a, b]             | a<-xs, b<-xs, b/=a] ++ 
                [[a, b, c]          | a<-xs, b<-xs, b/=a, c<-xs, c/=b, c/=a]
                -- [[a, b, c, d]       | a<-xs, b<-xs, b/=a, c<-xs, c/=b, c/=a, d<-xs, d/=c, d/=b, d/=a ] ++
                -- [[a, b, c, d, e]    | a<-xs, b<-xs, b/=a, c<-xs, c/=b, c/=a, d<-xs, d/=c, d/=b, d/=a, e<-xs, e/=d, e/=c, e/=b, e/=a] ++
                -- [[a, b, c, d, e, f] | a<-xs, b<-xs, b/=a, c<-xs, c/=b, c/=a, d<-xs, d/=c, d/=b, d/=a, e<-xs, e/=d, e/=c, e/=b, e/=a, f<-xs, f/=e, f/=d, f/=c, f/=b, f/=a]


choiceFun :: [a] -> [[a]]
choiceFun [] =  []
choiceFun fs =  [[a]                | a<-fs] ++
                [[a, b]             | a<-fs, b<-fs]
                -- [[a, b, c]          | a<-fs, b<-fs, c<-fs] ++ 
                -- [[a, b, c, d]       | a<-fs, b<-fs, c<-fs, d<-fs] ++
                -- [[a, b, c, d, e]    | a<-fs, b<-fs, c<-fs, d<-fs, e<-fs]


calc :: [Int] -> [Int->Int->Int] -> Int
calc [n] [] = n
calc (n:ns) (f:fs) = f (calc ns fs) n


-- 
process :: [Int] -> [Int->Int->Int] -> (Int->Int->Int) -> ([Int], [Int->Int->Int])
process (n:ns) (f:fs) op = 

-- solve :: [[a]] -> [[b]] -> [Int]
-- solve fs ns = [ f n | f<-fs, n<-ns, length f == (length n - 1) ]


-- ns = [1, 3, 7, 10, 25, 50]
ns = [3, 7, 10]
fs = [(+), (-), (*), div]
mm = calc [3, 7, 10] [(*), (-)]