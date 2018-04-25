module Bindings (idle, display, reshape, keyboardMouse) where
 
import Graphics.UI.GLUT
import Data.IORef
import Display
 
reshape :: ReshapeCallback
reshape size = do 
    viewport $= (Position 0 0, size)
 

rescale :: Position -> (Position, Size) -> (GLfloat, GLfloat)
rescale (Position x y) (Position xw yw, Size w h) =
    let xwF = fromIntegral xw
        ywF = fromIntegral yw
        wF = fromIntegral w
        hF = fromIntegral h
        xF = fromIntegral x
        yF = fromIntegral y in
            ((((2*(xF - xwF))/wF)-1), -(((2*(yF - ywF)) / hF)-1))

keyboardMouse :: IORef (GLfloat, GLfloat) -> KeyboardMouseCallback
keyboardMouse pos (MouseButton LeftButton) Down _ position = 
    do
        window <- get viewport
        putStrLn "clicked"
        pos $= (rescale position window)
        show_pos <- get pos
        putStrLn (show show_pos)
        putStrLn (show window)
keyboardMouse _ key place modifiers position = do
    putStrLn (show key)
    putStrLn (show place)
    putStrLn (show modifiers)
    window <- get viewport
    putStrLn (show position)
    putStrLn (show (rescale position window))
    return ()


