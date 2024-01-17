

data Nat = Zero | Succ Nat deriving Show

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

addnat :: Nat -> Nat -> Nat
addnat x y = int2nat (nat2int x + nat2int y)

mullnat :: Nat -> Nat -> Nat
mullnat Zero _ = Zero
mullnat x y = addnat (mullnat (int2nat (nat2int x - 1)) y) y


data Expr = Val Int | Add Expr Expr | Mul Expr Expr

size :: Expr -> Int
size (Val n) = n
size (Add x y) = size x + size y
size (Mul x y) = size x + size y

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y


-- folde :: 

data Tree a = Leaf a | Node (Tree a) (Tree a)

