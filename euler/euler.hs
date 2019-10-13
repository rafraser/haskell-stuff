-- | Problem #1
-- List all natural numbers below N that are multiples of 3 or 5
problem1 :: (Integral a) => a -> a
problem1 n = do
    sum [x | x <- [1..(n-1)], mod x 3 == 0 || mod x 5 == 0]

-- | Problem #2
-- Sum all of the even Fibonacci numbers that do not exceed N
problem2 :: (Integral a) => a -> a
problem2 n = do
    let fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)
    sum [x | x <- takeWhile (<= n) fibonacci, mod x 2 == 0]

-- | Problem #3
-- Find the largest prime factor of the number N
-- This is not working: still trying to find an efficient way to check if numbers are prime
-- This currently just finds the largest divisor of N
problem3 :: (Integral a) => a -> a
problem3 n = do
    let max = floor (sqrt (fromIntegral n))
    let divisors = [x | x <- [2..max], n `mod` x == 0]
    maximum divisors

-- | Problem #4
-- Find the largest palindrome made from the product of two 3-digit numbers
problem4 = maximum [x | p1 <- [100..999], p2 <- [100..999], let x = p1 * p2, show x == reverse (show x)]

-- | Problem #5
-- Find the largest prime number divisible by all numbers from 1 to N
problem5 n = foldr (lcm) 1 [1..n]

-- | Problem #6
-- Find the difference between the sum of the squares and the square of the sum for the first N natural numbers
problem6 :: (Integral a) => a -> a
problem6 n = do
    let sumSquares = sum [x^2 | x <- [1..n]]
    let squareSum = (sum [x | x <- [1..n]]) ^ 2
    squareSum - sumSquares

-- | Problem #7
-- Find the Nth prime
-- I'm too lazy to do a good prime testing thing right now so I'm leaving this blank
problem7 = 0

-- | Problem #8
-- Some grid thing I'm too lazy to type out right now

-- | Problem #9
-- Find a Pythagorean Triplet such that a^2 + b^2 = c^2 (where a, b, c are all natural)
-- There is exactly one triplet where a + b + c = 1000
-- Get the product of this triple
problem9 = do
    let triples = [[a, b, c] | a <- [1..1000], b <- [a..1000], let c = 1000 - a - b, (a^2) + (b^2) == (c^2)]
    product (triples !! 0)