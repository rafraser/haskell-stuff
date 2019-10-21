module MBGraphics3 where
import Data.Complex
import Graphics.Image as Image
import Mandelbrot
import MBGraphics2

renderHelper :: Double -> Image VS RGB Double
renderHelper radius = do
    let cx = -0.77568377
    let cy = 0.13646737
    let resolution = 512
    let max = 20
    let func = func_mandelbrot
    makeImageR VS (resolution, resolution) (pixelFunc cx cy radius max resolution func)

renderMandelbrotGIF = do
    let rate = 2 :: GifDelay
    let options = GIFSeqLooping LoopingForever 
    let frames = [(rate, renderHelper rad) | rad <- [2.00,1.95..0.05]]
    writeImageExact (Seq GIF) [options] "testing.gif" frames