type Pos = (Int, Int)

origin :: Pos
origin = (0,0)

left :: Pos -> Pos
left (x, y) = (x-1, y)

type Trans = Pos -> Pos

right :: Trans
right (x, y) = (x+1, y)

-----------------------------

type Pair a = (a,a)

mult :: Pair Int -> Int
mult (m, n) = m*n

copy :: a -> Pair a
copy x = (x,x)

-----------------------------

data Answer = Yes | No | Unknown

answers :: [Answer]
answers = [Yes, No, Unknown]

flip :: Answer -> Answer
flip Yes = Yes
flip No = Yes
flip Unknown = Unknown

-------------------------------

-- Constructors can also have paramaters

data Shape = Circle Float | Rect Float Float

square :: Float -> Shape
square n = Rect n n

rect :: Float -> Float -> Shape
rect x y = Rect x y

circle :: Float -> Shape
circle n = Circle n

area :: Shape -> Float
area (Circle r) = pi * r ^ 2
area (Rect x y) = y * y

--------------------------------

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv x y = Just (x `div` y)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead (x:xs) = Just x

---------------------------------

-- recursive types

data Nat = Zero | Succ Nat
    deriving Show

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat(n-1))

addNat :: Nat -> Nat -> Nat
addNat m n = int2nat(nat2int m + nat2int n) 

-- adding up nats using type recursion
addNat1 :: Nat -> Nat -> Nat
addNat1 Zero n = n 
addNat1 (Succ m) n = Succ (addNat1 m n)


----------------------------------

data Expr = Const Int | Add Expr Expr | Mul Expr Expr | Neg Expr
    deriving Show

test :: Expr
test = Add (Const 1) (Mul (Const 2) (Const 3))

size :: Expr -> Int
size (Const n) = 1
size (Add x y) = size x + size y
size (Mul x y) = size x + size y

eval :: Expr -> Int
eval (Const n) = n
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

-----------------------------------


multNat :: Nat -> Nat -> Nat
multNat Zero y = Zero
multNat (Succ x) y = addNat1 (multNat x y) y

------------------------------------

data Tree a = Leaf a | Node (Tree a) (Tree a )

------------------------------------

foldExpr :: (Int -> a) -> (a -> a -> a) -> (a -> a -> a) -> (a -> a) -> Expr -> a
foldExpr c a m n (Const x) = c x
foldExpr c a m n (Add e1 e2) = a (foldExpr c a m n e1) (foldExpr c a m n e2)
foldExpr c a m n (Mul e1 e2) = m (foldExpr c a m n e1) (foldExpr c a m n e2)
foldExpr c a m n (Neg e) = n (foldExpr c a m n e)