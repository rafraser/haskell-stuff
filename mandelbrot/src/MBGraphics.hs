-- | Mandelbrot Set
module MBGraphics where
import Data.Complex
import Graphics.Image as Image
import MBAscii

-- Constant number for iterations
-- This shouldn't be a constant but I'm a bad person
iterations = 50
-- Another constant for the image resolution
resolution = 512

-- | Clamps a number to be [0, 1]
clampColor :: (Ord a, Num a) => a -> a
clampColor a = max (min a 1) 0

getColor :: Int -> Pixel RGB Double
getColor a = do
    let redScale = 0.6
    let greenScale = 1.3
    let blueScale = 1.7
    let t = 1 * ((fromIntegral a) / (fromIntegral iterations))
    if a == iterations
        then PixelRGB 0 0 0
    else
        PixelRGB (clampColor (redScale * t)) (clampColor (greenScale * t)) (clampColor (blueScale * t))

-- | Iterate according to the Mandelbrot set formula
-- This will return a complex number corresponding to the ith iteration
-- The Mandelbrot set formula is:
-- z(n+1) = (zn)^2 + c
mandelbrotI :: (RealFloat a) => Complex a -> Int
mandelbrotI a = do
    let list = take (fromIntegral iterations) (iterate ((a +) . (^2)) 0)
    length (takeWhile (not . escaped) (list))

-- | Convert a point in the image (0..resolution, 0..resolution) to a point in the complex plane
pixelToComplex :: (RealFloat a) => Int -> Int -> Complex a
pixelToComplex i j = do
    let xx = fromIntegral(i) / fromIntegral(resolution)
    let yy = fromIntegral(j) / fromIntegral(resolution)
    let real = 4 * xx - 2
    let imag = 4 * yy - 2
    (real :+ imag)

pixelFunc :: (Int, Int) -> Pixel RGB Double
pixelFunc (i, j) = do
    getColor (mandelbrotI (pixelToComplex j i))

