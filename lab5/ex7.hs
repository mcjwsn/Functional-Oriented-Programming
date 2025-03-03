newtype Box a = MkBox a deriving Show

instance Applicative Box where
  pure = MkBox
  (MkBox f) <*> w = fmap f w
instance Functor Box where
  fmap f (MkBox x) = MkBox (f x)

data Tree a = Node a (Tree a) (Tree a)
            | Leaf

paths :: Tree a -> [[a]]
paths Leaf           = pure []
paths (Node a lt rt) = concat $ ([(a :)] <*>) <$> (fmap paths [lt, rt])
