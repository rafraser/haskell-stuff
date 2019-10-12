problem1 = sum [x | x <- [1..999], mod x 3 == 0 || mod x 5 == 0]

problem2 :: (Integral a) => a -> a
problem2 max = do
    let fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)
    sum [x | x <- takeWhile (<= max) fibonacci, mod x 2 == 0]

problem4 = maximum [x | p1 <- [100..999], p2 <- [100..999], let x = p1 * p2, show x == reverse (show x)]