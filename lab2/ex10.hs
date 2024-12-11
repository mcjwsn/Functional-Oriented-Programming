fst2Eq :: Eq a => [a] -> Bool
fst2Eq (x : y : _) | x == y = True
fst2Eq _                    = False

fdivs :: [Int] -> Bool
fdivs (x : y : _) | y `mod` x == 0 = True
fdivs _ = False