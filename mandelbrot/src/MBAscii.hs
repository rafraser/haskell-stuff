-- | Mandelbrot Set
module MBAscii where
import Data.Complex

-- | Return the squared magnitude of a given complex number
-- For our purposes of computing the Mandelbrot set, this is more efficient
-- Square roots are expensive!
magSquared :: RealFloat a => Complex a -> a
magSquared a = (Data.Complex.realPart a)^2 + (Data.Complex.imagPart a)^2

-- | Has a given complex number escaped?
-- A complex number has 'escaped' if the magnitude is greater than 2
-- For efficiency, this function checks if the *squared* magnitude is >= 4
escaped :: RealFloat a => Complex a -> Bool
escaped a = not (4 > (magSquared a))

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

-- | Generate an ASCII picture of the Mandelbrot Set
renderMandelbrotASCII = do
    mapM_ putStrLn [[pointCharacter (x :+ y) | x <- [-2,-1.9..2]] | y <- [-2,-1.9..2]]