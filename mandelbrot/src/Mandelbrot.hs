module Mandelbrot where
import Data.Complex

-- | Return the squared magnitude of a given complex number
-- For our purposes of computing the Mandelbrot set, this is more efficient
-- Square roots are expensive!
magSquared :: RealFloat a => Complex a -> a
magSquared a = (Data.Complex.realPart a)^2 + (Data.Complex.imagPart a)^2

-- | Has a given complex number escaped?
-- A complex number has 'escaped' if the magnitude is greater than 2
-- For efficiency, this function checks if the *squared* magnitude is >= 4
escaped :: Complex Double -> Bool
escaped a = not (4 > (magSquared a))

-- | Perform a Mandelbrot-esque iteration
-- There are many variations of the Mandelbrot set
-- This function iterations a given function until:
--   - the point escapes
--   - a maximum number of iterations has been reached
-- This returns the number of iterations until the given pointe escapes
mandelbrot :: Int -> Complex Double -> (Complex Double -> [Complex Double]) -> Int
mandelbrot max point func = do
    let list = take (max) (func point)
    length (takeWhile (not . escaped) (list))

-- | Standard Mandelbrot Set iterator
func_mandelbrot :: Complex Double -> [Complex Double]
func_mandelbrot a = iterate ((a +) . (^2)) a

-- | Julia Set iterator
func_julia :: Complex Double -> Complex Double -> [Complex Double]
func_julia c a = iterate((c +) . (^2)) a