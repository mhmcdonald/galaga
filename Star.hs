{-
   File      :  Star.hs
   Represents the stars in the background of the game. Their movement gives the
   appearance of inflight motion. 
-}
module Star
(
   Star(..),
   mkStar,
   starList,
   moveY,
   updateStar,
   renderStars
) where
import System.Random
import Data.Array.IO
import Control.Monad
import Graphics.Gloss
import GameConstants
import Data.List
import Graphics.Gloss.Interface.IO.Game

data Star = Star {getstar:: Picture, loc :: Point, flickering:: Bool} deriving (Eq)

mkStar :: Color -> Point -> Star
mkStar col (x,y) = Star (color col (circleSolid(2))) (x,y) False

colors :: [Color]
colors = [orange, yellow, white, blue]

starListO :: [Star]
starListO = [mkStar orange coord| coord <- initialCoords1]

starListY :: [Star]
starListY = [mkStar yellow coord| coord <- initialCoords2]

starListB :: [Star]
starListB = [mkStar blue coord| coord <- initialCoords3]

starListW :: [Star]
starListW = [mkStar white coord| coord <- initialCoord4]

starList :: [Star]
starList = starListO ++ starListY ++ starListB ++ starListW

renStar :: Star -> Picture
renStar star = translate (fst (loc star)) (snd (loc star)) (getstar star)


renderStars :: [Star] -> [Picture]
renderStars stars = do
                 listOfStars <-  map renStar stars
                 return listOfStars

moveY :: Star -> Float -> Star
moveY (Star pic (x,y) flickering ) dy = (Star pic newLoc flickering )
     where
       newLoc = (x, y+dy)

updateStar star = if snd(Star.loc star) < (-270.0)
                  then Star.moveY(star) 600.0
                  else Star.moveY(star) (-1.0)


initialCoords1 = [(350,-55),(-360,-180),(40,230),(330,-175),(-360,245),(100,-50),(-210,0),(170,-215),(-390,215),(-380,105),(80,-240),
                  (370,150),(-285,-265),(-160,-205),(135,140),(-100,-70),(40,-265),(-345,-210),(80,-60),(-215,230),(-50,170),
                  (-275,280),(205,310),(90,-100),(-125,-150),(-105,185),(-290,-85),(285,200),(-95,-135),(185,275),(-125,-70),
                  (-130,110),(-385,85),(110,-75),(365,-35),(-205,-235),(270,110),(195,85),(180,-165),(260,240),(300,280),(-40,10),
                  (-50,-225),(320,245),(-50,150),(215,160),(50,-135),(-15,75),(-200,195),(170,-230),(95,-230),(-390,5),(390,245)
                  ]

initialCoords2 = [(160,275),(-100,265),(370,-245),(75,-175),(-105,60),(-235,-115),(-190,115),(-110,70),(320,140),(65,25),
                  (-295,185),(275,-5),(-185,230),(350,235),(-65,-260),(50,150),(-390,-180),(205,300),(205,-270),(-70,235),(340,-85),
                  (365,-130),(135,60),(-180,100),(-360,-215),(-15,130),(150,250),(245,240),(-250,40),(210,-260),(-245,25),(-90,145),
                  (90,-115),(70,190),(300,110),(-70,-135),(395,30),(-180,140),(-400,55),(-75,265),(-300,-265),(50,30),(-340,25),
                  (140,-140),(-265,225),(-185,-40),(-285,185),(265,205),(-365,40),(200,-60),(-230,-185),(140,110),(355,120),(-385,235),
                  (350,-125),(255,245),(-145,-195),(85,0),(160,-135),(-180,-195),(225,-165),(370,235),(-315,80),(-280,235),(165,-25)
                  ]

initialCoords3 = [
                 (-390,-20),(30,-115),(210,-95),(220,290),(-220,-250),(-290,-100),(85,-110),(215,125),(280,40),(165,-85),(155,135),
                 (90,160),(360,225),(-230,-210),(285,-50),(-90,235),(155,35),(120,175),(90,-125),(-15,-160),(-165,245),(-155,110),
                 (-350,320),(375,-80),(-230,-160),(-50,-230),(395,75),(-45,295),(165,-235),(-220,210),(130,25),(-175,170),(-50,-240),
                 (-250,-160),(-245,80),(-10,-240),(-55,-260),(395,125),(385,10),(-25,-75),(45,145),(-400,-245),(135,-235),(-390,100),
                 (235,100),(25,90),(220,210),(225,-120),(-130,-75),(400,-45),(325,285),(-365,0),(-115,-230),(-95,-220),(-390,260),
                 (380,180),(-50,-270),(150,55),(-105,-265),(-335,300),(-360,-75),(-395,190),(340,65),(100,215),(-320,110),(-70,205)
                 ]

initialCoord4 = [
                 (-230,260),(-360,-260),(60,70),(-395,-15),(325,-160),(-275,40),(-295,165),(335,250),(160,-240),(95,0),(100,-35),
                 (-400,80),(-295,200),(-165,285),(-385,30),(-345,-15),(-390,-125),(195,65),(-205,-185),(-350,-45),(370,285),
                 (-250,145),(-175,20),(60,240),(355,-225),(-175,-195),(-60,185),(385,135),(-350,70),(-165,90),(-155,-80),(-345,30),
                 (175,100),(220,-85)
                ]
