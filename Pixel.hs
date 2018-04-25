import Graphics.UI.GLUT
import Data.IORef
import Bindings 

main :: IO ()
main = do
    (_progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    _window <- createWindow "Hello World"
    reshapeCallback $= Just reshape
    pos <- newIORef Nothing
    width <- newIORef 4
    keyboardMouseCallback $= Just (keyboardMouse pos width)
    displayCallback $= display pos width
    actionOnWindowClose $= MainLoopReturns
    mainLoop
    putStrLn "Bye now"
