module MBGraphics2 where
import Data.Complex
import Graphics.Image as Image
import Mandelbrot

-- | Clamps a number to be [0, 1]
clampColor :: (Ord a, Num a) => a -> a
clampColor a = max (min a 1) 0

-- | Return an RGB color given a number of iterations
-- This is done with a relatively simple calculation of scaling each channel by a constant
-- The default values for this provide a gorgeous light blue colour, with potent glows at the edges
getColor :: Int -> Int -> Pixel RGB Double
getColor max a = do
    let redScale = 0.6
    let greenScale = 1.3
    let blueScale = 1.7
    let t = 1 * ((fromIntegral a) / (fromIntegral max))
    if a == max
        then PixelRGB 0 0 0
    else
        PixelRGB (clampColor (redScale * t)) (clampColor (greenScale * t)) (clampColor (blueScale * t))

-- | Memoize the colors so they don't need to be computed for every pixel
memoized_colors :: Int -> Int -> Pixel RGB Double
memoized_colors max = (Prelude.map (getColor max) [0..max] !!)

-- | Convert a point in the image (0..resolution, 0..resolution) to a point in the complex plane
pixelToComplex :: Int -> Int -> Int -> Int -> Int -> Int -> Complex Double
pixelToComplex cx cy radius resolution i j = do
    let xx = fromIntegral(i) / fromIntegral(resolution)
    let yy = fromIntegral(j) / fromIntegral(resolution)
    let real = (2 * radius * xx) - radius + cx
    let imag = (2 * radius * yy) - radius + cy
    (real :+ imag)

-- | Iterate over each pixel of the image
-- i and j are the x and y coordinates of the image pixel respectively
pixelFunc :: Int -> Int -> Int -> Int -> Int -> (Complex Double -> [Complex Double]) -> (Int, Int) -> Pixel RGB Double
pixelFunc cx cy radius max resolution func (i, j) = do
    let p = pixelToComplex resolution j i
    memoized_colors max (mandelbrot max p func)

-- | Renders a graphical Mandelbrot function to a PNG file
-- This new renderer allows specifiying resolution, iterations, and function
renderMandelbrotPNG :: Int -> Int -> (Complex Double -> [Complex Double]) -> String -> IO()
renderMandelbrotPNG max resolution func filename = do
    let cx = 0
    let cy = 0
    let radius = 2

    let colors3 = makeImageR VS (resolution, resolution) (pixelFunc cx cy radius max resolution func) :: Image VS RGB Double
    writeImageExact PNG [] filename colors3