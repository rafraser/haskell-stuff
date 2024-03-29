import Graphics.UI.GLUT
import qualified Data.ByteString as B
import Data.IORef

points :: [(GLfloat, GLfloat, GLfloat)]
points = [(1, -1, 0), (-1, -1, 0), (-1, 1, 0), (1, -1, 0), (-1, 1, 0), (1, 1, 0)]

readCompileShader :: ShaderType -> FilePath -> IO Shader
readCompileShader st src = do
    src <- B.readFile src
    shader <- createShader st
    shaderSourceBS shader $= src
    compileShader shader
    return shader

loadShaders :: [Shader] -> IO()
loadShaders shaders = do
    program <- createProgram
    attachedShaders program $= shaders
    linkProgram program

    currentProgram $= Just program

    let setUniform var val = do
        location <- get (uniformLocation program var)
        uniform location $= val
    
    setUniform "resolution" (Vertex2 500 (500 :: GLfloat))

display :: DisplayCallback
display = do
    clear [ColorBuffer]

    renderPrimitive Triangles $ mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) points

    flush
    swapBuffers

getShaderName :: [String] -> String
getShaderName [] = "test"
getShaderName args = do head args

main :: IO()
main = do
    (name, args) <- getArgsAndInitialize
    initialDisplayMode $= [RGBMode, WithDepthBuffer, DoubleBuffered]
    initialWindowSize $= Size 500 500
    _ <- createWindow "Shader Test"

    let shaderName = getShaderName args

    -- Shaders?
    vs <- readCompileShader VertexShader ("shaders/" ++ shaderName ++ ".vert")
    fs <- readCompileShader FragmentShader ("shaders/" ++ shaderName ++ ".frag")
    loadShaders [vs, fs]

    displayCallback $= display

    mainLoop

    