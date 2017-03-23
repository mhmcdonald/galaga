{-
   File      :  Galaga.hs
   Represents the main starting point for the game
-}
module Main where

import Ship
import Star
import Lives
import Bullet
import Enemy
import EnemyBullet
import GameConstants
import Graphics.Gloss
import Data.List
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Point

data GameState    = MainMenu
                  | Game {ship :: Ship,
                          name :: String,
                          status :: String,
                          lives :: (Int, Float),
                          bullets :: [Bullet],
                          level :: Int,
                          clock :: Float,
                          score :: Float,
                          stars :: [Star],
                          enemies :: [Enemy],
                          eMovement :: (Dancing, Float),
                          eBullets:: ([EnemyBullet], Bool ),
                          attackingEnemies :: ([Enemy], Bool)
                          }
                  | HighScores
                  | GameOver deriving (Eq)


window :: Display
window = InWindow "Galaga" screenSize (10,10)

initState ::  GameState
initState = Game mkShip "Mark" "" (3, 0.0) [] 1 0.0 0 starList lev1Enemies (Not, 0.0) ([], False) ([], False)

-- The number of frames per second to render. Typically this is 60 fps
fps:: Int
fps = 60

main :: IO ()
main = do
    putStrLn "###################################################################"
    putStrLn "INSTRUCTIONS:"
    putStrLn "Use the left and right arrow keys to move. Press 'p' to restart from the beginning."
    putStrLn "Press the space bar to shoot the bad guys!"
    putStrLn "###################################################################"
    putStrLn "Enter your name and press ENTER to start saving the galaxy."
    nameGiven <- getLine
    highScores <- readFile $ "data/HighScore.txt"

    playIO window black fps initState Main.render eventHandler gameLoop


{- The render function takes a gamestate and renders it to the screen -}
render :: GameState -> IO Picture
render GameOver   =  return.scale 0.5 0.5.color white.text $ "Game Over"
render MainMenu   =  return.scale 0.3 0.3.color white.text $ "Main Menu"
render (Game  ship name status lives bullet l c scr str e d ebull aE) =   do
                               shipPicture <- Ship.render $ ship
                               livesList <- sequence . Lives.renderLives $ makeLives (fst lives)
                               enemyList <- sequence . Enemy.renderEnemies $ e
                               attackerList <- sequence . Enemy.renderEnemies $ (fst aE)
                               bullet <- sequence . Bullet.renderBullets $ bullet
                               enBullet <- sequence . EnemyBullet.renderEnemyBullets $ (fst ebull)
                               livesLabel <- Lives.renderLivesLabel
                               levelLabel <- Lives.renderLevelLabel
                               scoreLabel <- Lives.renderScoreLabel
                               lOfStars  <- return (renderStars str)
                               renderStatus <- Lives.renderStatus status
                               levelNum <- return.scale 0.15 0.15.color orange.text $ show (l)
                               scorePic <- return.scale 0.15 0.15.color orange.text $ show (round scr)
                               let score = translate (75.0) (-292.0) scorePic
                               let lev = translate(280.0) (-292.0) levelNum
                               let pics = pictures ([shipPicture, livesLabel, levelLabel, scoreLabel, score, lev, renderStatus] ++ livesList ++ lOfStars ++ bullet ++ enBullet ++ enemyList ++ attackerList)
                               return pics
render HighScores =  return.scale 0.5 0.5.color white.text $ "High Scores"




{- The event handlers handles events coming from the user -}
eventHandler :: Event -> GameState -> IO GameState
eventHandler (EventKey (Char key) Up _ _) state@(Game s name status lives bullets l c scr str e eMov eBull aE) =  case key of
        -- If the key is "q" then quit the game
        'p' -> return initState
        _  -> return state

{- This pattern matches for Special key Events-}
eventHandler (EventKey (SpecialKey key) Up _ _) state@(Game s name status lives bullets l c scr str e eMov eBull aE) = case key of
        -- Handles the events for the arrow keys
        KeySpace -> return (fireBullet state)
        KeyUp    -> return state
        KeyDown  -> return state
        KeyLeft  -> return (Game (Ship.moveX s (-20.0)) name status lives bullets l c scr str e eMov eBull aE)
        KeyRight -> return (Game (Ship.moveX s (20.0)) name status lives bullets l c scr str e eMov eBull aE)


{- This pattern matches for Mouse Events-}
eventHandler (EventKey (MouseButton _) Up _ (xPos,yPos)) state@(Game s name status lives bullets l c scr str e eMov eBull aE) = do
    putStrLn $  "Mouse Click on (" ++ show xPos ++ ", " ++ show yPos ++ ")"
    putStrLn $  "The time is: " ++ show c    -- this is temporary just to see how the time is working.
    return state


{- The catch all pattern -}
eventHandler _ state = return state


{- The game loop for Galaga -}
gameLoop :: Float -> GameState -> IO GameState
gameLoop deltaTime state@(Game ship name status lives bullet l c s str e eMov eBull aE) = do
        let newTime = c + deltaTime
        let timeState =  updateScore (deltaTime/2) (Game ship name status lives bullet l newTime s str e eMov eBull aE)
        let attackerState = fireEnBullet(moveAttackers(addAttackers(timeState)))
        let enemyState = moveEnemyFormation(attackerState)
        let liveState = moveStars(findBulEnmCollisions(enemyState)) --moves consistently
        let state' = findShipCollisions(liveState)
        let bulls = findShipandBullCollisions(state')
        return bulls


{- The updateScore function takes a gamestate and updates the number of points and level fields -}
updateScore :: Float -> GameState -> GameState
updateScore points state@(Game ship name status lives bullet l c s str e eMov eBull aE)
         | fst (lives) < 1 = state
         | otherwise = Game ship name status lives bullet nextLevel c newScore str enemyUpdates eNextMov eBull aE
                  where
                      newScore = s + points
                      nextLevel = if newScore >= 500
                                  then 4
                                  else if newScore >= 300
                                       then 3
                                       else if newScore >= 170
                                            then 2
                                            else 1
                      (enemyUpdates, eNextMov) = if l == 3 && nextLevel ==4
                                          then (lev4Enemies, (Not, 0.0))
                                          else if l == 4 && (length e) == 0
                                               then (lev4Enemies, (Not, 0.0))
                                               else if l == 2 && nextLevel ==3
                                                     then (lev3Enemies, (Not, 0.0))
                                                     else if l == 1 && nextLevel == 2
                                                          then (lev2Enemies, (Not, 0.0))
                                                          else (e, eMov)


{- The fireBullet function takes a gamestate and adds a ship's bullet to the gamestate based on the location of the ship. -}
fireBullet :: GameState -> GameState
fireBullet (Game ship name status lives bullet l c s str e eMov eBull aE) = Game ship name status lives (bullet ++ [newBullet]) l c s str e eMov eBull aE
     where
        location = Ship.loc ship   -- location of the ship
        newBullet = Bullet location (0,0)


{- An Attacker is an enemy which has left the formation and is pursuing the Ship. Only attackers fire bullets and attempt to
collide with the Ship. -}
addAttackers :: GameState -> GameState
addAttackers (Game ship name status lives bullet l c s str e eMov eBull aE)
        | fst lives > 0 && enemyCount > 0 && attackEnemies < 1 && l < 2 && snd aE == False = (Game ship name status lives bullet l c s str newEnemyL eMov eBull (attackers, True))
        | fst lives > 0 && enemyCount > 0 && attackEnemies < 2  && snd aE == False = (Game ship name status lives bullet l c s str newEnemyL eMov eBull (attackers, True))
        | fst lives > 0 =  (Game ship name status lives bullet l c s str e eMov eBull (fst aE, False))
        | otherwise = (Game ship name status lives bullet l c s str e eMov eBull aE)
            where
                attackEnemies = length (fst aE)
                enemyCount = length e
                selectedEnemy = e !!((length e) `div` 2)
                attackers = (fst aE) ++ [selectedEnemy]
                newEnemyL = delete selectedEnemy e

{- Attackers only get introduced into the game after 5 seconds of play. The attackers pursue the ship. Rather than pursue in a straight
line, they move towards the ship. As levels progress, the attackers move towards the ship faster.-}
moveAttackers :: GameState -> GameState
moveAttackers state@(Game ship name status lives bullet l c s str e eMov eBull aE)
      | c > 5 && fst lives > 0 = Game ship name status lives bullet l c s str e eMov enemyB (moveAE, snd aE)
      | fst lives > 0 = state
      | otherwise = Game ship name "gameover" lives [] l c s str e eMov ([], False) ([], snd aE)
     where
       enemyB = (map (\x -> EnemyBullet.moveY x (-3.5) ) (filter (\x -> (snd(EnemyBullet.loc x)) > (-305)) (fst eBull)), (snd eBull))
       moveAE = map (\x -> checkTarget ship x l) (fst aE)

{- The fireEnBullet function takes a gamestate and adds an enemy's bullet to the gamestate based on the location of the enemy. The enemies
will fire every 7 seconds. -}
fireEnBullet :: GameState -> GameState
fireEnBullet state@(Game ship name status lives bullet l c s str e eMov eBull aE)
          | l < 3 && fst lives > 0 && snd eBull == False && (round c) `mod` 7 == 0 = Game ship name status lives bullet l c s str e eMov (((map (\x -> mkEnemyBullet (Enemy.loc x)) (fst aE) ) ++ (fst eBull)), True) aE
          | l < 3 && fst lives > 0 && snd eBull == True && (round c) `mod` 7 == 0 = state
          | l > 2 && fst lives > 0 && snd eBull == False && (round c) `mod` 3 == 0 = Game ship name status lives bullet l c s str e eMov (((map (\x -> mkEnemyBullet (Enemy.loc x)) (fst aE) ) ++ (fst eBull)), True) aE
          | l > 2 && fst lives > 0 && snd eBull == True && (round c) `mod` 3 == 0 = state
          | fst lives > 0 = Game ship name status lives bullet l c s str e eMov ((fst eBull), False) aE
          | otherwise = state

{- checkTarget is a helper function of moveAttackers. It assists in moving the enemy attacker towards the Ship.-}
checkTarget :: Ship -> Enemy -> Int -> Enemy
checkTarget s eB level
        | level == 1 && (fst ( Ship.loc s)) > (fst (Enemy.loc eB)) = updateEnemy(Enemy.moveX(eB) 0.15)
        | level == 1 && (fst (Ship.loc s)) < (fst (Enemy.loc eB)) = updateEnemy(Enemy.moveX(eB) (-0.15))
        | level == 2 && (fst ( Ship.loc s)) > (fst (Enemy.loc eB)) = updateEnemy(Enemy.moveX(eB) 0.20)
        | level == 2 && (fst (Ship.loc s)) < (fst (Enemy.loc eB)) = updateEnemy(Enemy.moveX(eB) (-0.20))
        | level > 2 && (fst ( Ship.loc s)) > (fst (Enemy.loc eB)) = updateEnemy(Enemy.moveX(eB) 0.30)
        | level > 2 && (fst (Ship.loc s)) < (fst (Enemy.loc eB)) = updateEnemy(Enemy.moveX(eB) (-0.30))
        | otherwise = eB

{-moveBullets is the function which has move's the ship's bullets vertically. If the bullets reach the top of the screen, they get removed
from play.-}
moveBullets :: GameState -> GameState
moveBullets (Game ship name status lives bullet l c s str e eMov eBull aE) = Game ship name status lives newBullet l c s str e eMov eBull aE
             where
                newBullet = map (\x -> Bullet.moveY(x) 8.0) (filter (\x -> (snd(Bullet.loc x)) < 300 ) bullet)  -- remove bullets t

{-moveStars animates the background display of the game by moving the stars vertically.-}
moveStars :: GameState -> GameState
moveStars (Game ship name status lives bullet l c s str e eMov eBull aE) = Game ship name status lives bullet l c s newStars e eMov eBull aE
         where
            fixedStars = map updateStar str
            newStars = fixedStars

{-moveEnemyFormation is the function which takes a gamestate, moves the Enemys in a clockwise circular pattern on a timed basis.-}
moveEnemyFormation :: GameState -> GameState
moveEnemyFormation state@(Game ship name status lives bullet l c s str e eMov eBull aE)
     | l  == 1 && c > 7 && (round c) `mod` 13 == 0 && fst eMov == Not = (Game ship name status lives bullet l c s str dancingER (Enemy.Right, c) eBull aE)
     | l  == 2 && (round c) `mod` 11 == 0 && fst eMov == Not = (Game ship name status lives bullet l c s str dancingER (Enemy.Right, c) eBull aE)
     | l  == 3 && (round c) `mod` 5 == 0 && fst eMov == Not = (Game ship name status lives bullet l c s str dancingER (Enemy.Right, c) eBull aE)
     | l  == 4 && (round c) `mod` 2    == 0 && fst eMov == Not = (Game ship name status lives bullet l c s str dancingER (Enemy.Right, c) eBull aE)
     | fst eMov == Enemy.Right && (c - snd eMov) < 0.75 = (Game ship name status lives bullet l c s str dancingER (Enemy.Right, snd eMov) eBull aE)
     | fst eMov == Enemy.Right && (c - snd eMov) > 1.2 = (Game ship name status lives bullet l c s str dancingED (Enemy.Bottom, c) eBull aE)
     | fst eMov == Enemy.Bottom && (c - snd eMov) < 1.0 = (Game ship name status lives bullet l c s str dancingED (Enemy.Bottom, snd eMov) eBull aE)
     | fst eMov == Enemy.Bottom && (c - snd eMov) > 2.0 = (Game ship name status lives bullet l c s str dancingEL (Enemy.Left, c) eBull aE)
     | fst eMov == Enemy.Left && (c - snd eMov) < 1.0 = (Game ship name status lives bullet l c s str dancingEL (Enemy.Left, snd eMov) eBull aE)
     | fst eMov == Enemy.Left && (c - snd eMov) > 2.0 = (Game ship name status lives bullet l c s str dancingET (Enemy.Top, c) eBull aE)
     | fst eMov == Enemy.Top && (c - snd eMov) < 1.0 = (Game ship name status lives bullet l c s str dancingET (Enemy.Top, snd eMov) eBull aE)
     | fst eMov == Enemy.Top && (c - snd eMov) > 2.5 = (Game ship name status lives bullet l c s str dancingER (Enemy.ReturnCenter, c) eBull aE)
     | fst eMov == Enemy.ReturnCenter && (c - snd eMov) < 0.9 = (Game ship name status lives bullet l c s str dancingER (Enemy.ReturnCenter, snd eMov) eBull aE)
     | fst eMov == Enemy.ReturnCenter && (c - snd eMov) > 2.0 = (Game ship  name status lives bullet l c s str e (Enemy.Not, 0.0) eBull aE)
     | otherwise = state
        where
           dancingER = (map (\x -> Enemy.moveX(x) 1.5) e)
           dancingED = (map (\x -> Enemy.moveY(x) (-2.0)) e)
           dancingEL = (map (\x -> Enemy.moveX(x) (-2.5)) e)
           dancingET = (map (\x -> Enemy.moveY(x) (2.0)) e)

{-addStars is a function which repeatedly move stars that have gone off the screen to the top of the screen.-}
addStars :: GameState -> GameState
addStars (Game ship name status lives bullet l c s str e eMov eBull aE) = Game ship name status lives bullet l c s newStars e eMov eBull aE
     where
       topStars = map (\x -> Star.moveY(x) 300.0) (filter (\x -> (snd(Star.loc x)) < (-200) ) str)
       newStars = topStars

{-findBulEnmCollisions function finds collisions between Bullets and enemies. It removes these dead enemys from the gamestate-}
findBulEnmCollisions :: GameState -> GameState
findBulEnmCollisions (Game ship name status lives bullet l c s str e eMov eBull aE)
        | length (bulletRemoval) > 0 = moveBullets(Game ship name status lives freshBullets l c (s + scoreIncrease) str freshEnemy eMov eBull (freshAttackers, snd aE))
        | otherwise = moveBullets(Game ship name status lives bullet l c s str e eMov eBull aE)
     where
        collidedBandE = (collideds bullet (e ++ (fst aE)))
        bulletRemoval = (fst collidedBandE)
        enemyRemoval = (snd collidedBandE)
        scoreIncrease = ((fromIntegral (length enemyRemoval) )) * 10.0
        freshBullets = removerB bulletRemoval bullet
        freshEnemy = removerE enemyRemoval e
        freshAttackers = removerE enemyRemoval (fst aE)

{-findShipCollisions function uses collision detection to identify when the Ship and an Enemy attacker have collided. After the ship
is hit by either a bullet or an enemy ship, there is a 3 second grace period, during which if the ship gets hit again, no life is lost. -}
findShipCollisions :: GameState -> GameState
findShipCollisions state@(Game ship name status lives bullet l c s str e eMov eBull aE)
        | length newEnemys > 0 && (snd lives) == 0.0 = (Game explodingShip1 name status (removeLife, c) bullet l c s str e eMov eBull (newEnemys, False))
        | (snd lives) > 0.0 && c - (snd lives) < 0.4  = (Game explodingShip1 name status lives [] l c s str e eMov eBull aE)
        | (snd lives) > 0.0 && c - (snd lives) < 1.0  = (Game explodingShip2 name status lives [] l c s str e eMov eBull aE)
        | (snd lives) > 0.0 && c - (snd lives) < 1.8  = (Game explodingShip3 name status lives [] l c s str e eMov eBull aE)
        | (snd lives) > 0.0 && c - (snd lives) < 2.0  = (Game regularShip name status lives [] l c s str e eMov eBull aE)
        | (snd lives) > 0.0 && c - (snd lives) > 2.0  = (Game regularShip name status (fst lives, 0.0) bullet l c s str e eMov eBull aE)
        | otherwise = (Game ship name status lives bullet l c s str e eMov eBull aE)
     where
        collidedSandE = (collidedsWe ship (fst aE))
        newEnemys = removerE (snd (collidedSandE)) (fst aE)
        removeLife = (fst lives) - 1
        explodingShip1 = Ship {Ship.loc = Ship.loc ship , isExploding = "ex1"}
        explodingShip2 = Ship {Ship.loc = Ship.loc ship , isExploding = "ex2"}
        explodingShip3 = Ship {Ship.loc = Ship.loc ship , isExploding = "ex3"}
        regularShip = Ship { Ship.loc = Ship.loc ship, isExploding = ""}


{-findShipandBullCollisions function uses collision detection to identify when the Ship and an Enemy bullet have collided. After the ship
is hit by either a bullet or an enemy ship, there is a 3 second grace period, during which if the ship gets hit again, no life is lost. -}
findShipandBullCollisions :: GameState -> GameState
findShipandBullCollisions state@(Game ship name status lives bullet l c s str e eMov eBull aE)
        | length newEBull > 0 && (snd lives) == 0.0 = (Game explodingShip1 name status (removeLife, c) bullet l c s str e eMov (newEBull, True) aE)
        |  (snd lives) > 0.0 && c - (snd lives) < 0.4 = (Game explodingShip1 name status lives [] l c s str e eMov ([], True) aE)
        |  (snd lives) > 0.0 && c - (snd lives) < 1.0 = (Game explodingShip2 name status lives [] l c s str e eMov ([], True) aE)
        |  (snd lives) > 0.0 && c - (snd lives) < 1.8 = (Game explodingShip3 name status lives [] l c s str e eMov ([], True) aE)
        |  (snd lives) > 0.0 && c - (snd lives) < 2.0 = (Game regularShip name status lives [] l c s str e eMov ([], True) aE)
        |  (snd lives) > 0.0 && c - (snd lives) > 2.0 = (Game regularShip name status (fst lives, 0.0) bullet l c s str e eMov (newEBull, True) aE)
        | otherwise = (Game ship name status lives bullet l c s str e eMov eBull aE)
     where
        collidedSandEB = (collidedsWeB ship (fst eBull))
        newEBull = removerEB (snd (collidedSandEB)) (fst eBull)
        removeLife = (fst lives) - 1
        explodingShip1 = Ship {Ship.loc = Ship.loc ship , isExploding = "ex1"}
        explodingShip2 = Ship {Ship.loc = Ship.loc ship , isExploding = "ex2"}
        explodingShip3 = Ship {Ship.loc = Ship.loc ship , isExploding = "ex3"}
        regularShip = Ship { Ship.loc = Ship.loc ship, isExploding = ""}

{-helper function to collisionShipWEnemy-}
collidedsWe :: Ship -> [Enemy] -> ([Ship], [Enemy])
collidedsWe s ens = unzip (collisionShipWEnemy ( [(f, d) | f <- [s], d <- ens]))

{-helper function to collidedsWeB-}
collidedsWeB :: Ship -> [EnemyBullet] -> ([Ship], [EnemyBullet])
collidedsWeB s ensb = unzip (collisionShipWEnemyBull ( [(f, d) | f <- [s], d <- ensb]))

{-helper function to collideds-}
collideds :: [Bullet] -> [Enemy] -> ([Bullet], [Enemy])
collideds bs ens = unzip (collisionDbulletWenemy ( [(f, d) | f <- bs, d <- ens]))

{-helper function to remover Bullets which have collided-}
removerB :: [Bullet] -> [Bullet] -> [Bullet]
removerB garbage list = concat(map (\x -> delete x list) garbage)

{-helper function to remove Enemys which collided-}
removerE :: [Enemy] -> [Enemy] -> [Enemy]
removerE garbage list = concat(map (\x -> delete x list) garbage)

{-helper function to remove Enemy Bullets which collided-}
removerEB :: [EnemyBullet] -> [EnemyBullet] -> [EnemyBullet]
removerEB garbage list = concat(map (\x -> delete x list) garbage)

{-collisionDbulletWenemy takes a list of tuples and finds those bullets and enemies which have collided.-}
collisionDbulletWenemy :: [(Bullet, Enemy)] -> [(Bullet,Enemy)]
collisionDbulletWenemy zippedColliders = filter  didCollide zippedColliders

{-collisionShipWEnemy takes a list of tuples and finds those ships and enemies which have collided.-}
collisionShipWEnemy :: [(Ship, Enemy)] -> [(Ship, Enemy)]
collisionShipWEnemy z = filter didCollideSwE z

{-collisionShipWEnemyBull takes a list of tuples and finds those ships and enemy bullets which have collided.-}
collisionShipWEnemyBull :: [(Ship, EnemyBullet)] -> [(Ship, EnemyBullet)]
collisionShipWEnemyBull z = filter didCollideSwEB z

{-didCollideSwE returns a boolean of True when a Bullet and an Enemy have collided.-}
didCollide :: (Bullet, Enemy) -> Bool
didCollide (bullet@(Bullet {Bullet.loc = (bX,bY)}), enemy@(Enemy {Enemy.loc = (eX,eY)})) =
                          if (pointInBox (Bullet.loc bullet)  ((eX + (-11.0)),(eY + 11.0))  ((eX + 11.0), (eY + (-11.0))))  == True
                          then True
                          else False


{-didCollideSwE returns a boolean of True when a Ship and an Enemy have collided.-}
didCollideSwE :: (Ship, Enemy) -> Bool
didCollideSwE (ship@(Ship {Ship.loc = (bX,bY)}), enemy@(Enemy {Enemy.loc = (eX,eY)})) =
                          if (pointInBox (Ship.loc ship)  ((eX + (-25.0)),(eY + 25.0))  ((eX + 25.0), (eY + (-25.0))))  == True
                          then True
                          else False

{-didCollideSwEB returns a boolean of True when a Ship and an EnemyBullet have collided.-}
didCollideSwEB :: (Ship, EnemyBullet) -> Bool
didCollideSwEB (ship@(Ship {Ship.loc = (bX,bY)}), enemy@(EnemyBullet {EnemyBullet.loc = (eX,eY)})) =
                          if (pointInBox (Ship.loc ship)  ((eX + (-25.0)),(eY + 25.0))  ((eX + 25.0), (eY + (-25.0))))  == True
                          then True
                          else False
