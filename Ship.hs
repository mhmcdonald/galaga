module Ship
(
   Ship(..),
   mkShip,
   moveX,
   render
) where

import Graphics.Gloss
import GameConstants
import Data.List

data Ship = Ship {loc ::Point, isExploding :: String } deriving (Eq)

render :: Ship -> IO Picture
render ship = do
    if isExploding ship == ""
    then do
       pic <- shipPic
       return $ translate (fst (loc ship) + 10) (snd (loc ship)) pic
    else if isExploding ship == "ex1"
          then do
               pic2 <- ex1Pic
               return $ translate (fst (loc ship)) (snd (loc ship)) pic2
          else if isExploding ship == "ex2"
               then do
                    pic3 <- ex2Pic
                    return $ translate (fst (loc ship)) (snd (loc ship)) pic3
               else do
                    pic4 <- ex3Pic
                    return $ translate (fst (loc ship)) (snd (loc ship)) pic4


shipPic :: IO Picture
shipPic = loadBMP "images/models/ship.bmp"

ex1Pic :: IO Picture
ex1Pic = loadBMP "images/models/frame1.bmp"

ex2Pic :: IO Picture
ex2Pic = loadBMP "images/models/frame2.bmp"

ex3Pic :: IO Picture
ex3Pic = loadBMP "images/models/frame3.bmp"

bulletPic :: IO Picture
bulletPic = loadBMP "images/models/ship_bullet.bmp"

mkShip :: Ship
mkShip = Ship (-370 ,-240 ) ""


moveX :: Ship -> Float -> Ship
moveX s@(Ship {loc = (x,y)}) dx = if x + dx < (-380)
                                   then s {loc = ((-380), y)}
                                   else if x + dx > 380
                                        then s {loc = (380, y)}
                                        else s {loc = (x+dx, y)}
