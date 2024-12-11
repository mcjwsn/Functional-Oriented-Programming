data BinIntTree = EmptyIntBT |
                  IntNodeBT Int BinIntTree BinIntTree

sumBinIntTree :: BinIntTree -> Int
sumBinIntTree EmptyIntBT = 0
sumBinIntTree (IntNodeBT n lt rt) = n + sumBinIntTree lt + sumBinIntTree rt

data BinTree a = EmptyBT |
                 NodeBT a (BinTree a) (BinTree a)

sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyBT = 0
sumBinTree (NodeBT n lt rt) = n + sumBinTree lt + sumBinTree rt

data Expr a = Lit a | -- literal/value a, e.g. Lit 2 = 2
              Add (Expr a) (Expr a) |
              Substract (Expr a) (Expr a)|
              Multiply (Expr a) (Expr a)

eval :: Num a => Expr a -> a
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Substract e1 e2) = eval e1 - eval e2
eval (Multiply e1 e2) = eval e1 * eval e2

show' :: Show a => Expr a -> String
show' (Lit n) = show n
show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"
show' (Substract e1 e2) = "(" ++ show' e1 ++ "-" ++ show' e2 ++ ")"
show' (Multiply e1 e2) = "(" ++ show' e1 ++ "*" ++ show' e2 ++ ")"

depthOfBT :: BinTree a -> Int
depthOfBT EmptyBT = 0
depthOfBT (NodeBT _ lt rt) =  1 + max (depthOfBT lt) (depthOfBT rt)

flattenBTPreorder :: BinTree a -> [a]
flattenBTPreorder EmptyBT = []
flattenBTPreorder (NodeBT x lt rt) = 
    [x] ++ flattenBTPreorder lt ++ flattenBTPreorder rt

flattenBTInorder :: BinTree a -> [a]
flattenBTInorder EmptyBT = []
flattenBTInorder (NodeBT x lt rt) = 
    flattenBTInorder lt ++ [x] ++ flattenBTInorder rt

flattenBTPostorder :: BinTree a -> [a]
flattenBTPostorder EmptyBT = []
flattenBTPostorder (NodeBT x lt rt) = 
    flattenBTPostorder lt ++ flattenBTPostorder rt ++ [x]

mapBT :: (a -> b) -> BinTree a -> BinTree b -- funkcja map dla drzewa binarnego
mapBT _ EmptyBT = EmptyBT
mapBT f (NodeBT x lt rt) = NodeBT (f x) (mapBT f lt) (mapBT f rt)

 