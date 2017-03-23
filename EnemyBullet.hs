{-
   File      :  EnemyBullet.hs
   Represents the enemy attacking ship's bullets that travell vertically downward
   and threaten the main ship.
-}
module EnemyBullet
(
   EnemyBullet(..),
   mkEnemyBullet,
   renderEnemyBullets,
   moveY,
   moveX,
   render
) where

import Graphics.Gloss
import GameConstants
import Data.List

data EnemyBullet = EnemyBullet {loc ::Point} deriving (Eq, Ord, Show)

render :: EnemyBullet -> IO Picture
render bullet = do
       pic <- bulletPic
       return $ translate (fst (loc bullet)) (snd (loc bullet)) pic

renderEnemyBullets :: [EnemyBullet] -> [IO Picture]
renderEnemyBullets bullets = do
                 listOfBullets <-  map render bullets
                 return listOfBullets

bulletPic :: IO Picture
bulletPic = loadBMP "images/models/enemy_bullet.bmp"

mkEnemyBullet :: Point -> EnemyBullet
mkEnemyBullet loc = EnemyBullet loc

moveY :: EnemyBullet -> Float -> EnemyBullet
moveY b@(EnemyBullet {loc = (x,y)}) dy = b {loc = (x, y+dy)}

moveX :: EnemyBullet -> Float -> EnemyBullet
moveX e@(EnemyBullet {loc = (x,y)}) dx = if x + dx < (-380)
                                         then e {loc = ((-380), y)}
                                         else if x + dx > 380
                                              then e {loc = (380, y)}
                                              else e {loc = (x+dx, y)}
