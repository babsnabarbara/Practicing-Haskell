import Data.List (delete)

data Op = Add | Sub | Mul | Div
    deriving (Show)
data Expr = Val Int | App Op Expr Expr
    deriving (Show)

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Mul x y = (x <= y) && (x/=1) && (y/= 1)
valid Sub x y = x > y
valid Div x y = x `mod` y == 0 && y /= 1

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y ]


subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where
                  yss = subs xs

inss :: a -> [a] -> [[a]]
inss x [] = [[x]]
inss x (y:ys) = (x:y:ys) : map (y:) (inss x ys)

perms :: Eq a => [a] -> [[a]]
perms [] = [[]]
perms xs = [ys | x <- xs
               , zs <- perms (delete x xs)
               , ys <- inss x zs]

choices :: Eq a => [a] -> [[a]]
choices xs = [zs | ys <- subs xs, zs <- perms ys]

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

check_solution :: Expr -> [Int] -> Int -> Bool
check_solution e ns n = elem (values e) (choices ns) && eval e == [n]

split :: [a] -> [([a], [a])]
split xs = [(take n xs,  drop n xs)| n <- ns]
    where 
        ns = [1..(length xs) -1]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- [Add, Sub, Mul, Div]]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [ e | (ls, rs) <- split ns
                    , l <- exprs ls
                    , r <- exprs rs
                    , e <- combine l r]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns
                    , e <- exprs ns'
                    , eval e == [n]]

type Result = (Expr, Int)


combine' :: Result -> Result -> [Result]
combine' (l, x) (r, y) =
    [ (e, v)
    | o <- [Add, Sub, Mul, Div]
    , e <- [App o l r]
    , v <- eval e
    ]

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [res | (ls, rs) <- split ns
                , lx <- results ls
                , ry <- results rs
                , res <- combine' lx ry]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = 
                [e | ns' <- choices ns
                , (e, m) <- results ns'
                , m == n]