sqr x = x^2

funcFactory n = case n of
 1 -> id
 2 -> sqr
 3 -> (^3)
 4 -> \x -> x^4
 5 -> intFunc
 _ -> const n
 where
   intFunc x = x^5

expApproxUpTo :: Int -> Double -> Double
expApproxUpTo n x = sum [x ^ k / fromIntegral (product [1..k]) | k <- [0..n]]