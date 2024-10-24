absInt :: Int -> Int
absInt n | n >= 0 = n
	| otherwise = -n

sgn :: Int -> Int
sgn n | n>=0 = 1
	|otherwise =0 
	
min2Int :: (Int, Int) -> Int
min2Int (x,y) | x > y = y
	| otherwise = x

min3Int :: (Int, Int, Int) -> Int
min3Int (x,y,z) | x > y = min2Int(y,z)
		| otherwise = min2Int(x,z)


isDigit :: Char -> Bool
isDigit a | fromEnum(a)>47 && fromEnum(a)<58 = True
	| otherwise = False

