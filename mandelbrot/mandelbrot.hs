import Data.Complex

-- | Return the squared magnitude of a given complex number
-- For our purposes of computing the Mandelbrot set, this is more efficient
-- Square roots are expensive!
magSquared :: RealFloat a => Complex a -> a
magSquared a = (realPart a)^2 + (imagPart a)^2

-- | Has a given complex number escaped?
-- A complex number has 'escaped' if the magnitude is greater than 2
-- For efficiency, this function checks if the *squared* magnitude is >= 4
escaped :: RealFloat a => Complex a -> Bool
escaped a = (magSquared a) > 4

-- | Iterate according to the Mandelbrot set formula
-- This will return a complex number corresponding to the ith iteration
-- The Mandelbrot set formula is:
-- z(n+1) = (zn)^2 + c
mandelbrot :: RealFloat a => Complex a -> Int -> Complex a
mandelbrot a i = iterate ((a +) . (^2)) 0 !! i

-- | Converts a complex number to a character based on whether it escaped
pointCharacter :: RealFloat a => Complex a -> Char
pointCharacter a
    | escaped p = ' '
    | otherwise = '*'
    where
        p = mandelbrot a 50