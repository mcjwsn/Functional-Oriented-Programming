import Control.Exception
import System.IO

testProd :: Int -> IO ()
testProd n = do
  putStrLn $ "Testing n = " ++ show n
  print $ sum'3 (replicate n 1)

main :: IO ()
main = catch (testProd 100000) handleOverflow
  where
    handleOverflow :: SomeException -> IO ()
    handleOverflow e = putStrLn $ "Overflow detected: " ++ show e

sum'3 :: Num a => [a] -> a
sum'3 = loop 0
 where loop acc []     = acc
       loop acc (x:xs) = loop (x + acc) xs

isOdd :: (Ord a, Num a) => a -> Bool
isOdd n | n <= 0    = False
        | otherwise = isEven (n-1)

isEven :: (Ord a, Num a) => a -> Bool
isEven n | n < 0     = False
         | n == 0    = True
         | otherwise = isOdd (n-1)

ackerFun m n
 | m == 0    = n + 1
 | n == 0    = ackerFun (m - 1) 1
 | otherwise = ackerFun (m - 1) (ackerFun m (n - 1))
         