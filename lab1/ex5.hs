sgn :: Int -> Int
sgn n = if n < 0
       then -1
       else if n == 0
            then 0
            else 1

absInt :: Int -> Int
absInt n = if n> 0
	then n
	else -n

min2Int :: (Int, Int) -> Int
min2Int (a,b) = if a > b
	then b
	else a

min3Int :: (Int, Int,Int) -> Int
min3Int (a,b,c) = min2Int(min2Int(a,b),c)

toUpper :: Char -> Char
toUpper a = toEnum(-32 + fromEnum(a))

toLower :: Char -> Char
toLower a = toEnum(32+fromEnum(a))

isDigit :: Char -> Bool
isDigit a = if fromEnum(a)>47
	then if fromEnum(a)<58
	then True
	else False
	else False

charToNum :: Char -> Int
charToNum a = if fromEnum(a)>47
	then if fromEnum(a)<58
	then fromEnum(a) - 48
	else -1
	else -1


