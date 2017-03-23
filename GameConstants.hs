{- 
   File      :  GameConstants.hs 
   Represents the constants values that will not change through out the execution of the game. 
-}
module GameConstants 
(
    screenSize, 
    screenWidth,
    screenHeight,
    Size 
)where 

import Graphics.Gloss

type Size = (Int,Int)

screenSize :: Size 
screenSize = (800,600)

screenWidth :: Int 
screenWidth = fst screenSize

screenHeight :: Int 
screenHeight = snd screenSize