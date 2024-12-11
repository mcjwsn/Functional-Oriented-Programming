newtype MyInt = MkMyInt Int
instance Eq MyInt where
  (==) (MkMyInt i1) (MkMyInt i2) = i1 == i2
instance Ord MyInt where
  (<=) (MkMyInt i1) (MkMyInt i2) = i1 <= i2
instance Num MyInt where
  (+) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 + i2)
  (-) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 - i2)
  (*) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 * i2)
  negate (MkMyInt i)            = MkMyInt (negate i)
  abs (MkMyInt i)               = MkMyInt (abs i)
  signum (MkMyInt i)            = MkMyInt (signum i)
  fromInteger int               = MkMyInt (fromIntegral int)
instance Show MyInt where
    show (MkMyInt i) = "MkMyInt " ++ show i

instance Eq a => Eq (BinTree a) where
    Empty == Empty = True
    Node v1 l1 r1 == Node v2 l2 r2 = v1 == v2 && l1 == l2 && r1 == r2
    _ == _ = False
