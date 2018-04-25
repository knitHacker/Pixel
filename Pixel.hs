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
    keyboardMouseCallback $= Just (keyboardMouse pos)
    displayCallback $= display pos
    actionOnWindowClose $= MainLoopReturns
    mainLoop
    putStrLn "Bye now"
