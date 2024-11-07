fib :: (Num a, Eq a) => a -> a
fib n =
 if n == 0 || n == 1 then n
 else fib (n - 2) + fib (n - 1)

sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

prod' :: Num a=> [a] ->a
prod'[] = 1
prod' (x:xs) = x * prod' xs

length' :: [a]-> Int
length'[] = 0
length'(_:xs) = 1 + length' xs

or' :: [Bool] -> Bool
or'[] = False
or' (x:xs) = x || or' xs
