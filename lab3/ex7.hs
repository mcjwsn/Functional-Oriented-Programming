import Data.Char (isUpper)

onlyEven [] = []
onlyEven (x:xs)
 | x `mod` 2 == 0 = x : onlyEven xs
 | otherwise      = onlyEven xs

onlyOdd [] = []
onlyOdd (x:xs)
 | x `mod` 2 == 1 = x : onlyOdd xs
 | otherwise      = onlyOdd xs

onlyUpper [] = []
onlyUpper (x:xs)
 | isUpper(x) == True = x: onlyUpper xs
 | otherwise = onlyUpper xs

filter' :: (a->Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs) 
    | p x       = x : filter' p xs 
    | otherwise = filter' p xs