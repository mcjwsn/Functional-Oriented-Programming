roots :: (Double, Double, Double) -> (Double, Double)
roots (a, b, c) = ( (-b - d) / e, (-b + d) / e )
   where d = sqrt (b * b - 4 * a * c)
         e = 2 * a
{-
koem
-}
--kom


collatz :: Int -> Int
collatz n =
    let divides d x = x `mod` d == 0
        isEven x = divides 2 x  -- Użycie funkcji divides do sprawdzenia parzystości
        result = if isEven n
                 then n `div` 2
                 else 3 * n + 1
    in result