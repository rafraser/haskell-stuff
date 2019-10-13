-- | Mandelbrot Set
module Main where
import Data.Complex
import MBAscii
import MBGraphics

-- | Create a rendering of the Mandelbrot Set when this program is run
main :: IO()
main = do
    renderMandelbrotASCII