not' :: Bool -> Bool
not' b = case b of
	True -> False
	False -> True

absInt n =
	case(n>=0) of 
	True -> n
	_ -> -n

isItTheAnswer :: String -> Bool
isItTheAnswer b = case b of
	"Love" -> True
	_ -> False

or' :: (Bool, Bool) -> Bool
or' b = case b of
	(False, False)->False
	_ -> True

and' :: (Bool, Bool) -> Bool
and' b = case b of
	(True, True)->True
	_ -> False

nand' :: (Bool, Bool) -> Bool
nand' b = case b of
	(True, True)->False
	_ -> True

xor' :: (Bool, Bool) -> Bool
xor' b = case b of
	(True, False) -> True  	
	(False, True) -> True    
	_             -> False