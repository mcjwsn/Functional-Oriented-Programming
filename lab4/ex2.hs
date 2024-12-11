data CartInt2DVec = MkCartInt2DVec Int Int 
xCoord :: CartInt2DVec -> Int
xCoord (MkCartInt2DVec x _) = x

yCoord :: CartInt2DVec -> Int
yCoord (MkCartInt2DVec _ y) = y



data Cart3DVec a = Cart3DVec
  { xCoord3 :: a
  , yCoord3 :: a
  , zCoord3 :: a
  } deriving (Show, Eq)


data Cart2DVec' a = MkCart2DVec' a a

xCoord' :: Cart2DVec' a -> a
xCoord' (MkCart2DVec' x _) = x

yCoord' :: Cart2DVec' a -> a
yCoord' (MkCart2DVec' _ y) = y

data Cart2DVec'' a = MkCart2DVec'' {x::a, y::a}


data List a = EmptyL | Cons a (List a) deriving Show

head' :: List a -> a
head' EmptyL      = error "head': the empty list has no head!"
head' (Cons x xs) = x


data TrafficLights = Red |
                    Yellow |
                    Green

type Move = String
actionFor :: TrafficLights -> Move
actionFor Red = "Stop"
actionFor Yellow = "get ready"
actionFor Green = "Go"

data Shape = Circle Float          
           | Rectangle Float Float 
           deriving (Show, Eq)

area :: Shape -> Float
area (Circle r)          = pi * r^2
area (Rectangle width height) = width * height
