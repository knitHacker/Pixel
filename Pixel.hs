import Graphics.UI.GLUT
import Data.IORef
import Bindings 

main :: IO ()
main = do
    (_progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    _window <- createWindow "Hello World"
    reshapeCallback $= Just reshape
    pos <- newIORef (0, 0)
    keyboardMouseCallback $= Just (keyboardMouse pos)
    displayCallback $= display pos
    idleCallback $= Just idle
    mainLoop
