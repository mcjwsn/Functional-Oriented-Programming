import Data.Char

doubleElems []     = []
doubleElems (x:xs) = 2 * x : doubleElems xs

sqrElems [] = []
sqrElems (x:xs) = x^2 : sqrElems xs

lowerCase [] = []
lowerCase(x:xs) = toLower(x) : lowerCase xs


map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f(x:xs) = f x : map' f xs

doubleElems2 = map' (*2)
sqrElems2    = map' (^2)
lowerCase2  = map' toLower

doubleElems3 xs = [2 * x | x <- xs]

sqrElems3 xs = [x^2 | x <- xs]

lowerCase3 xs = [toLower x | x <- xs]