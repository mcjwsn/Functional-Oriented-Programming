module Stack (Stack, push, pop) where

isEmpty :: Stack a -> Bool
push :: a -> Stack a -> Stack a
pop :: Stack a -> (a,Stack a)

newtype Stack a = MkStack [a] deriving Show

isEmpty (MkStack s) = null s
push x (MkStack s) = MkStack (x:s)s
pop (MkStack (s:ss)) = (s,MkStack ss)