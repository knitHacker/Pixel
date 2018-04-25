module Display (idle, display, Coord2) where
 
import Graphics.UI.GLUT
import Control.Monad
import Data.IORef
 

vertex2f :: (GLfloat, GLfloat) -> IO()
vertex2f (x, y) = vertex $ Vertex2 x y

square :: GLfloat -> PrimitiveMode -> IO()
square w mode = let p = w / 2 in
    renderPrimitive mode $ mapM_ vertex2f
        [( p, p), ( p,-p), (-p, -p), (-p, p)]

type Coord2 = (GLfloat, GLfloat)

data Square = Square GLfloat GLfloat GLfloat GLfloat

squares :: Int -> GLfloat -> [Square]
squares n w = squares' xPoints yPoints

    where subSquareW = w / (fromIntegral n)
          xPoints = subPoints (-(w/2))
          yPoints = subPoints (-(w/2))
          subPoints subW = let newSub = subW + subSquareW in
                if subW <= (w/2) then subW : (subPoints newSub)
                                 else []
          squares' (x1:x2:xs) ys = (rows x1 x2 ys) ++ (squares' (x2:xs) ys)
          squares' _ _ = []
          rows x1 x2 (y1: y2: ys') = (Square x1 x2 y1 y2) : (rows x1 x2 (y2:ys'))
          rows _ _ _ = []


drawSquare :: PrimitiveMode -> Square -> IO()
drawSquare mode (Square x1 x2 y1 y2) = do
    renderPrimitive mode $ mapM_ vertex2f
        [(x1, y1), (x1, y2), (x2, y2), (x2, y1)]

findSquare :: GLfloat -> GLfloat -> [Square] -> Maybe Square
findSquare x y [] = Nothing
findSquare x y (s@(Square x1 x2 y1 y2):tl) = 
    if x > x1 && x < x2 && y > y1 && y < y2 then Just s
                                            else findSquare x y tl



display :: IORef (Maybe (Either Coord2 Coord2)) -> DisplayCallback
display pos = let subSquares = squares 5 1.0 in do
    clear [ColorBuffer, DepthBuffer] -- clear depth buffer too
    clear [ColorBuffer]
    loadIdentity
    color $ Color3 1 1 (1::GLfloat)
    square 1.0 Quads
    color $ Color3 1 0 (0::GLfloat)
    mapM_ (drawSquare LineLoop) subSquares
    maybePos <- get pos
    case maybePos of
        Nothing -> return ()
        (Just e) -> do
            case e of
                (Left (x',y')) -> do
                    color $ Color3 0 1 (0::GLfloat)
                    case findSquare x' y' subSquares of
                        Nothing -> return ()
                        (Just square) -> drawSquare Quads square
                (Right (x', y')) -> do
                    color $ Color3 0 0 (1::GLfloat)
                    case findSquare x' y' subSquares of
                        Nothing -> return ()
                        (Just square) -> drawSquare Quads square
    swapBuffers


idle :: IdleCallback
idle = do
    postRedisplay Nothing
