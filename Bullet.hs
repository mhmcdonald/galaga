{-
   File      :  Bullet.hs
   Represents the main ship's bullets which travel from bottom to top of the screen
   in a linear fashion. Bullets which surpass the top of the screen are discarded.
-}
module Bullet
(
   Bullet(..),
   mkBullet,
   renderBullets,
   moveY,
   render
) where

import Graphics.Gloss
import GameConstants
import Data.List

data Bullet = Bullet {loc ::Point, dim::Size} deriving (Eq, Ord, Show)

render :: Bullet -> IO Picture
render bullet = do
       pic <- bulletPic
       return $ translate (fst (loc bullet)) (snd (loc bullet)) pic

renderBullets :: [Bullet] -> [IO Picture]
renderBullets bullets = do
                 listOfBullets <-  map render bullets
                 return listOfBullets

bulletPic :: IO Picture
bulletPic = loadBMP "images/models/ship_bullet.bmp"

mkBullet :: Point -> Bullet
mkBullet loc = Bullet loc (0,0)

moveY :: Bullet -> Float -> Bullet
moveY b@(Bullet {loc = (x,y)}) dy = b {loc = (x, y+dy)}
