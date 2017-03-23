{-
   File      :  Lives.hs
   Represents the ship's lives that it has throughout the progression of the game.
   Everytime a collision occurs with the ship, the number of lives is decremented. 
-}
module Lives
(
   Lives,
   Label,
   mkLife,
   mkLabel,
   makeLives,
   renderLives,
   renderLivesLabel,
   renderLevelLabel,
   renderScoreLabel,
   renderStatus,
   render
) where

import Graphics.Gloss
import GameConstants
import Data.List

data Lives = Lives {loc ::Point, dim::Size} deriving (Eq, Ord, Show)
data Label = Label {locL ::Point, dimL::Size}

render :: Lives -> IO Picture
render life = do
    pic <- shipPic
    return $ translate (fst (loc life)) (snd (loc life)) pic

renderLives :: [Lives] -> [IO Picture]
renderLives lives = do
                 listOfPics <-  map render lives
                 return listOfPics

renderStatus :: String -> IO Picture
renderStatus status = do
                       pic1 <- gameOverPic
                       if status == ""
                       then do
                             return (translate (0.0) (210.0) blank)
                       else do
                             return (translate (0.0) (200.0) pic1)


renderLivesLabel:: IO Picture
renderLivesLabel = do
          pic <- llabelPic
          return (translate (-340.0) (-285.0) pic)


renderScoreLabel:: IO Picture
renderScoreLabel = do
          pic <- slabelPic
          return (translate (-20.0) (-285.0) pic)

renderLevelLabel:: IO Picture
renderLevelLabel = do
          pic <- levlabelPic
          return (translate (220.0) (-285.0) pic)

shipPic :: IO Picture
shipPic = loadBMP "images/models/ship.bmp"

llabelPic :: IO Picture
llabelPic = loadBMP "images/models/lives.bmp"

slabelPic :: IO Picture
slabelPic = loadBMP "images/models/score.bmp"

levlabelPic :: IO Picture
levlabelPic = loadBMP "images/models/level.bmp"

gameOverPic :: IO Picture
gameOverPic = loadBMP "images/models/endInstructions.bmp"

mkLife :: Lives
mkLife = Lives (-370.0, -280.0) (0, 0)

mkLabel :: Label
mkLabel = Label (-310.0, -280.0) (0, 0)

makeLives :: Int -> [Lives]
makeLives count
  | count == 3 =  [ Lives (-270.0, -280.0) (0,0), Lives (-240.0, -280.0) (0,0), Lives (-210.0, -280.0) (0,0)]
  | count == 2 =  [ Lives (-270.0, -280.0) (0,0), Lives (-240.0, -280.0) (0,0)]
  | count == 1 =  [ Lives (-270.0, -280.0) (0,0)]
  | otherwise = []
