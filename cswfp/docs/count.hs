count :: Eq a => a -> [a] -> Int
count x [] = 0
count x (y:ys) | x == y = succ (count x ys)
               | otherwise = count x ys
