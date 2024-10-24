myFun :: Num a => a -> a
myFun x = 2 * x

add2T :: Num a => (a, a) -> a
add2T (x,y) = x + y

add2C :: Num a => a -> a -> a
add2C x y = x + y

add3T :: Num a=> (a,a,a) -> a
add3T (x,y,z) = x + y + z

add3C :: Num a=> a-> a-> a->a
add3C x y z = x+y+z


fiveToPower_ :: Integer->Integer
fiveToPower_ = (5^)

subtrNFrom5_ :: Num a=> a->a
subtrNFrom5_ = (5-)

substr5From_ :: Num a=> a-> a
substr5From_ n = n-5