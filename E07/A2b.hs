g :: Int -> Int -> Int
g a 0 = a
g a b
  | b == 1 = g (a + 1) (b - 1)
  | otherwise = g (a + 2) (b - 2)
