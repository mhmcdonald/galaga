{-
   File      :  Enemy.hs
   Represents the enemy ships that rotate around the screen and attack the
   ship throughout the execution of the game. 
-}
module Enemy
(
   Fighter(..),
   Dancing(..),
   Enemy(..),
   renderEnemies,
   render,
   moveY,
   moveX,
   updateEnemy,
   lev1Enemies,
   lev2Enemies,
   lev3Enemies,
   lev4Enemies
) where

import Graphics.Gloss
import GameConstants
import Data.List

data Fighter = Blue | Red deriving (Eq, Ord, Show)
data Dancing = Not | Left | Right | Top | Bottom | ReturnCenter deriving (Eq, Ord, Show)

data Enemy = Enemy {loc ::Point, typeOf::Fighter, attackPoint::Point}
            | Nil deriving (Eq, Ord, Show)
            {-moves the enemy horizontally -}

{-moves the enemy vertically -}
moveY :: Enemy -> Float -> Enemy
moveY e@(Enemy {loc = (x,y)}) dy = if y + dy > 295
                                   then e {loc = (x, 295)}
                                   else e {loc = (x, y + dy)}


{-moves the enemy horizontally -}
moveX :: Enemy -> Float -> Enemy
moveX e@(Enemy {loc = (x,y)}) dx = if x + dx < (-380)
                                   then e {loc = ((-380), y)}
                                   else if x + dx > 380
                                        then e {loc = (380, y)}
                                        else e {loc = (x+dx, y)}

{-if an enemy navigates off screen, we bring them back into the game at the top of the screen using
this function -}
updateEnemy en = if snd(Enemy.loc en) < (-330.0)
                  then Enemy.moveY(en) 600.0
                  else Enemy.moveY(en) (-2.0)


render :: Enemy -> IO Picture
render enemy = do
    pic <- if (typeOf enemy) == Blue then bluePic else redPic
    return $ translate (fst (loc enemy)) (snd (loc enemy)) pic

renderEnemies :: [Enemy] -> [IO Picture]
renderEnemies enemies = do
                 listOfPics <-  map render enemies
                 return listOfPics


redPic :: IO Picture
redPic = loadBMP "images/models/red_fighter.bmp"


bluePic :: IO Picture
bluePic = loadBMP "images/models/blue_fighter.bmp"

enemyBulletPic :: IO Picture
enemyBulletPic = loadBMP "images/models/enemy_bullet.bmp"

lev1Enemies :: [Enemy]
lev1Enemies = intialEnemiesR3 ++ intialEnemiesR4 ++ intialEnemiesR5 ++ intialEnemiesR6



lev2Enemies :: [Enemy]
lev2Enemies = intialEnemiesR2 ++ intialEnemiesR3 ++ intialEnemiesR4 ++ intialEnemiesR5 ++ intialEnemiesR6

lev3Enemies :: [Enemy]
lev3Enemies = intialEnemiesR1 ++ intialEnemiesR2 ++ intialEnemiesR3
                 ++ intialEnemiesR4 ++ intialEnemiesR5 ++ intialEnemiesR6

lev4Enemies :: [Enemy]
lev4Enemies = intialEnemiesR0 ++ intialEnemiesR1 ++ intialEnemiesR2 ++ intialEnemiesR3
                 ++ intialEnemiesR4 ++ intialEnemiesR5 ++ intialEnemiesR6

{-The following are the initial formations of enemy ships. -}
intialEnemiesR0 = [
                  Enemy {loc = (-275.5, 180.5),  typeOf=Red, attackPoint=(0.0,-340.0)},
                  Enemy {loc = (-225.5, 180.5),  typeOf=Blue, attackPoint=(0.0,-340.0)},
                  Enemy {loc = (-175.5, 180.5),  typeOf=Red, attackPoint=(0.0,-340.0)},
                  Enemy {loc = (-125.5, 180.5),  typeOf=Blue, attackPoint=(0.0,-340.0)},
                  Enemy {loc = (-75.5, 180.5),  typeOf=Red, attackPoint=(0.0,-340.0)},
                  Enemy {loc = (-25.5, 180.5),  typeOf=Blue, attackPoint=(0.0,-340.0)},
                  Enemy {loc = (25.5, 180.5),  typeOf=Red, attackPoint=(0.0,-340.0)},
                  Enemy {loc = (75.5, 180.5),  typeOf=Blue, attackPoint=(0.0,-340.0)},
                  Enemy {loc = (125.5, 180.5),  typeOf=Red, attackPoint=(0.0,-340.0)},
                  Enemy {loc = (175.5, 180.5),  typeOf=Blue, attackPoint=(0.0,-340.0)},
                  Enemy {loc = (225.5, 180.5),  typeOf=Red, attackPoint=(0.0,-340.0)},
                  Enemy {loc = (275.5, 180.5),  typeOf=Blue, attackPoint=(0.0,-340.0)}
                ]


intialEnemiesR1 = [
                  Enemy {loc = (-275.5, 150.5),  typeOf=Blue, attackPoint=(0.0,-340.0)},
                  Enemy {loc = (-225.5, 150.5),  typeOf=Red, attackPoint=(0.0,-340.0)},
                  Enemy {loc = (-175.5, 150.5),  typeOf=Blue, attackPoint=(0.0,-340.0)},
                  Enemy {loc = (-125.5, 150.5),  typeOf=Red, attackPoint=(0.0,-340.0)},
                  Enemy {loc = (-75.5, 150.5),  typeOf=Blue, attackPoint=(0.0,-340.0)},
                  Enemy {loc = (-25.5, 150.5),  typeOf=Red, attackPoint=(0.0,-340.0)},
                  Enemy {loc = (25.5, 150.5),  typeOf=Blue, attackPoint=(0.0,-340.0)},
                  Enemy {loc = (75.5, 150.5),  typeOf=Red, attackPoint=(0.0,-340.0)},
                  Enemy {loc = (125.5, 150.5),  typeOf=Blue, attackPoint=(0.0,-340.0)},
                  Enemy {loc = (175.5, 150.5),  typeOf=Red, attackPoint=(0.0,-340.0)},
                  Enemy {loc = (225.5, 150.5),  typeOf=Blue, attackPoint=(0.0,-340.0)},
                  Enemy {loc = (275.5, 150.5),  typeOf=Red, attackPoint=(0.0,-340.0)}
                ]

intialEnemiesR2 = [
                  Enemy {loc = (-225.5, 120.5),  typeOf=Blue, attackPoint=(0.0,-340.0)},
                  Enemy {loc = (-175.5, 120.5),  typeOf=Red, attackPoint=(0.0,-340.0)},
                  Enemy {loc = (-125.5, 120.5),  typeOf=Blue, attackPoint=(0.0,-340.0)},
                  Enemy {loc = (-75.5, 120.5),  typeOf=Red, attackPoint=(0.0,-340.0)},
                  Enemy {loc = (-25.5, 120.5),  typeOf=Blue, attackPoint=(0.0,-340.0)},
                  Enemy {loc = (25.5, 120.5),  typeOf=Red, attackPoint=(0.0,-340.0)},
                  Enemy {loc = (75.5, 120.5),  typeOf=Blue, attackPoint=(0.0,-340.0)},
                  Enemy {loc = (125.5, 120.5),  typeOf=Red, attackPoint=(0.0,-340.0)},
                  Enemy {loc = (175.5, 120.5),  typeOf=Blue, attackPoint=(0.0,-340.0)},
                  Enemy {loc = (225.5, 120.5),  typeOf=Red, attackPoint=(0.0,-340.0)}
                ]


intialEnemiesR3 = [
                  Enemy {loc = (-175.5, 90.5),  typeOf=Blue, attackPoint=(0.0,-340.0)},
                  Enemy {loc = (-125.5, 90.5),  typeOf=Red, attackPoint=(0.0,-340.0)},
                  Enemy {loc = (-75.5, 90.5),  typeOf=Blue, attackPoint=(0.0,-340.0)},
                  Enemy {loc = (-25.5, 90.5),  typeOf=Red, attackPoint=(0.0,-340.0)},
                  Enemy {loc = (25.5, 90.5),  typeOf=Blue, attackPoint=(0.0,-340.0)},
                  Enemy {loc = (75.5, 90.5),  typeOf=Red, attackPoint=(0.0,-340.0)},
                  Enemy {loc = (125.5, 90.5),  typeOf=Blue, attackPoint=(0.0,-340.0)},
                  Enemy {loc = (175.5, 90.5),  typeOf=Red, attackPoint=(0.0,-340.0)}
                ]


intialEnemiesR4 = [
                  Enemy {loc = (-125.5, 60.5),  typeOf=Blue, attackPoint=(0.0,-340.0)},
                  Enemy {loc = (-75.5, 60.5),  typeOf=Red, attackPoint=(0.0,-340.0)},
                  Enemy {loc = (-25.5, 60.5),  typeOf=Blue, attackPoint=(0.0,-340.0)},
                  Enemy {loc = (25.5, 60.5),  typeOf=Red, attackPoint=(0.0,-340.0)},
                  Enemy {loc = (75.5, 60.5),  typeOf=Blue, attackPoint=(0.0,-340.0)},
                  Enemy {loc = (125.5, 60.5),  typeOf=Red, attackPoint=(0.0,-340.0)}
                ]

intialEnemiesR5 = [
                  Enemy {loc = (-75.5, 30.5),  typeOf=Blue, attackPoint=(0.0,-340.0)},
                  Enemy {loc = (-25.5, 30.5),  typeOf=Red, attackPoint=(0.0,-340.0)},
                  Enemy {loc = (25.5, 30.5),  typeOf=Blue, attackPoint=(0.0,-340.0)},
                  Enemy {loc = (75.5, 30.5),  typeOf=Red, attackPoint=(0.0,-340.0)}
                ]

intialEnemiesR6 = [
                  Enemy {loc = (-25.5, 0.5),  typeOf=Blue, attackPoint=(0.0,-340.0)},
                  Enemy {loc = (25.5, 0.5),  typeOf=Red, attackPoint=(0.0,-340.0)}
                ]
