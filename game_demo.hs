module Main where

import Graphics.UI.Fungen
import Graphics.Rendering.OpenGL (GLdouble)

-------------
-- Ventana --
-------------
width = 600
height = 500
w = fromIntegral width :: GLdouble
h = fromIntegral height :: GLdouble
winConfig = ((0,0),(width,height),"Pong")

-----------------------------
-- Definiciones de estados --
-----------------------------
data GameAttribute = Score {current :: Int, high :: Int}

---------------- 
-- Constantes --
---------------- 
initSpeed = 2 :: GLdouble
initPos   = (w/2,h/2)
initScore = Score 0 0
-------------
-- Objetos --
-------------
-- Ball
ball = objectGroup "ballGroup" [createBall]

createBall :: GameObject () -- objectAttrib
createBall =
  let ballPic = Basic (Circle 6.0 1 0.5 0.0 Filled)
  in object "ball" ballPic False initPos (2,2) ()  

-- Bar
bar = objectGroup "barGroup"  [createBar]

createBar :: GameObject ()
createBar =
  let barBound = [(-25,-6),(25,-6),(25,6),(-25,6)]
      barPic   = Basic (Polyg barBound 1.0 1.0 1.0 Filled)
  in object "bar" barPic False (w/2,30) (0,0) ()


moveBarToRight :: Modifiers -> Position -> IOGame GameAttribute () () () ()
moveBarToRight _ _ = do
  obj     <- findObject "bar" "barGroup"
  (pX,pY) <- getObjectPosition obj
  (sX,_)  <- getObjectSize obj
  if (pX + (sX/2) + 5 <= w)
   then (setObjectPosition ((pX + 5),pY) obj)
   else (setObjectPosition ((w - (sX/2)),pY) obj)

moveBarToLeft :: Modifiers -> Position -> IOGame GameAttribute () () () ()
moveBarToLeft _ _ = do
  obj <- findObject "bar" "barGroup"
  (pX,pY) <- getObjectPosition obj
  (sX,_)  <- getObjectSize obj
  if (pX - (sX/2) - 5 >= 0)
    then (setObjectPosition ((pX - 5),pY) obj)
    else (setObjectPosition (sX/2,pY) obj)

---------------
-- Controles --
---------------
input = [(SpecialKey KeyRight, StillDown, moveBarToRight),
         (SpecialKey KeyLeft,  StillDown, moveBarToLeft),
         (Char 'q',   Press,     \_ _ -> funExit)]

---------------
-- Main loop --
---------------
gameCycle :: IOGame GameAttribute () () () ()
gameCycle = do
  (Score current high) <- getGameAttribute

  ball <- findObject "ball" "ballGroup"

  col1 <- objectLeftMapCollision ball
  col2 <- objectRightMapCollision ball
  when (col1 || col2) (reverseXSpeed ball)

  col3 <- objectTopMapCollision ball
  when col3 (reverseYSpeed ball)

  col4 <- objectBottomMapCollision ball
  when col4 $ do
    setGameAttribute (Score 0 high)
    setObjectPosition initPos ball
    setObjectSpeed (initSpeed,initSpeed) ball

  bar <- findObject "bar" "barGroup"
  col5 <- objectsCollision ball bar
  let (_,vy) = getGameObjectSpeed ball
  when (col5 && vy < 0) $ do
    (x,y) <- getObjectSpeed ball
    setObjectSpeed (x*1.2,-y*1.2) ball
    let new_current = current + 10
    setGameAttribute (Score new_current (if new_current > high then new_current else high))

  printOnScreen ("Puntaje mas alto: " ++ show high)  Helvetica18 (5,h-15) 1.0 1.0 1.0
  printOnScreen ("Puntaje actual:   " ++ show current) Helvetica18 (5,h-30) 1.0 1.0 1.0
  showFPS Helvetica18 (w-20,5) 1.0 1.0 1.0

--------------------
-- Fondo del mapa --
--------------------
bmpList = [("spacebg.bmp", Nothing)]
gameMap = textureMap 0 200 200 w h -- el 0 hace referencia al primer elemento de bmpList

---------
-- FPS --
---------
target_fps = 60 
tick = 1000 `div` target_fps

-------------------
-- Juntando todo --
-------------------
main :: IO ()
main = funInit winConfig gameMap [bar,ball] () initScore input gameCycle (Timer tick) bmpList
