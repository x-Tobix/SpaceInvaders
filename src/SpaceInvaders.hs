module SpaceInvaders
    ( Spaceship (..)
    , Invader (..)
    , Invaders
    , GameAction (..)
    , Game (..)
    , Missile (..)
    , Missiles
    , InvadersMissiles
    , startLevel
    , update
    , actionHandler
    , missileRadius
    , AppState(..)
    , InvaderDirection(..)
    , removeItem
    , isEmpty
    , Rectangle(..)
    , Circle(..)
    , recCircIntersect
    , removeIf
    , moveInvadersMissiles
    , moveInvaders
    , moveMissiles
    , moveSpaceshipRight
    , moveSpaceshipLeft
    ) where

import System.Random ( uniformR, mkStdGen, StdGen )
import Graphics.Gloss.Interface.IO.Game ()
import Graphics.Gloss ()
import Data.Bifunctor ( Bifunctor(first) )
import GHC.Float ( int2Float )


----------BASE----------
data AppState = Menu | Running | Lost
  deriving (Eq, Show)

type World = (Float, Float)

----------HELPERS----------
data Rectangle = Rectangle World Float Float
  deriving (Eq, Show)

data Circle = Circle World Float
  deriving (Eq, Show)

----------SPACE INVADERS GAME STATE----------
data Missile = Missile
  { positionMis :: World
  , velocity :: Float
  , radius :: Float
  } deriving (Eq, Show)

missileRadius :: Float
missileRadius = 5

newtype Spaceship = Spaceship World
  deriving (Eq, Show)

data InvaderDirection = LeftDir | RightDir
  deriving (Eq, Show)

data Invader = Invader
  { positionInv :: World
  , startX :: Float
  , direction :: InvaderDirection
  }
  deriving (Eq, Show)

type Invaders = [Invader]

type Missiles = [Missile]

type InvadersMissiles = [Missile]

data GameAction = Quit | PushLeft | PushRight | UnpushLeft | UnpushRight | Shoot | Pause | Start | Restart
  deriving (Eq, Show)

data Game = Game
  { appstate :: AppState
  , spaceship :: Spaceship
  , invaders :: Invaders
  , movements :: [GameAction]
  , missiles :: Missiles
  , invadersMissiles :: InvadersMissiles
  , randomGenerator :: StdGen
  , invaderAttackChance :: Float
  , score :: Int
  , pause :: Bool
  , level :: Int
  }
  deriving (Eq, Show)

----------GAME ACTION HANDLERS----------
actionHandler :: GameAction -> Game -> Game
actionHandler Restart game@(Game Lost _ _ _ _ _ _ _ _ _ _) = startLevel Menu 1 0 (0, -270) [] []
actionHandler Restart game = game
actionHandler _ game@(Game Lost _ _ _ _ _ _ _ _ _ _) = game
actionHandler Start game@(Game Menu spaceship invaders movs mis emis g c s p l) = Game Running spaceship invaders movs mis emis g c s p l
actionHandler Start game@(Game Running _ _ _ _ _ _ _ _ _ _) = game
actionHandler _ game@(Game Menu _ _ _ _ _ _ _ _ _ _) = game
actionHandler Quit _ = startLevel Menu 1 0 (0, -270) [] []
actionHandler PushLeft (Game Running spaceship invaders movs mis emis g c s p l) = Game Running spaceship invaders (movs ++ [PushLeft]) mis emis g c s p l
actionHandler PushRight (Game Running spaceship invaders movs mis emis g c s p l) = Game Running spaceship invaders (movs ++ [PushRight]) mis emis g c s p l
actionHandler UnpushRight (Game Running spaceship invaders movs mis emis g c s p l) = Game Running spaceship invaders (removeItem PushRight movs) mis emis g c s p l
actionHandler UnpushLeft (Game Running spaceship invaders movs mis emis g c s p l) = Game Running spaceship invaders (removeItem PushLeft movs) mis emis g c s p l
actionHandler Shoot (Game Running (Spaceship (x,y)) invaders movs mis emis g c s p l) = Game Running (Spaceship (x,y)) invaders movs (mis ++ [Missile (x,y + 40) 10 missileRadius]) emis g c s p l
actionHandler Pause (Game Running spaceship invaders movs mis emis g c s p l) = Game Running spaceship invaders movs mis emis g c s (not p) l


----------GAME FUNCTIONS----------
startLevel :: AppState -> Int -> Int -> (Float, Float) -> [GameAction] -> InvadersMissiles -> Game
startLevel state l sc pos movs emis = Game
  { appstate = state
  , spaceship = Spaceship pos
  , invaders = [ Invader (-400, 280) (-400) LeftDir, Invader (-300, 280) (-300) LeftDir, Invader (-200, 280) (-200) LeftDir,
    Invader (-100, 280) (-100) LeftDir, Invader (0, 280) 0 LeftDir, Invader (100, 280) 100 LeftDir, Invader (200, 280) 200 LeftDir,
    Invader (300, 280) 300 LeftDir, Invader (400, 280) 400 LeftDir, Invader (-400, 200) (-400) LeftDir, Invader (-300, 200) (-300) LeftDir,
    Invader (-200, 200) (-200) LeftDir, Invader (-100, 200) (-100) LeftDir, Invader (0, 200) 0 LeftDir, Invader (100, 200) 100 LeftDir,
    Invader (200, 200) 200 LeftDir, Invader (300, 200) 300 LeftDir, Invader (400, 200) 400 LeftDir]
  , movements = movs
  , missiles = emis
  , invadersMissiles = []
  , randomGenerator = mkStdGen 2137
  , invaderAttackChance = int2Float (l - 1) * 0.01 + 0.1
  , score = sc
  , pause = False
  , level = l
  }

update :: Float -> Game -> Game
update _ game@Game {appstate = state, spaceship = Spaceship(t1,t2), invaders = invs, movements = movs, missiles = mis, invadersMissiles = emis, randomGenerator = g, invaderAttackChance = chance, score = sc, pause = p, level = l}
  | state == Menu || state == Lost || p = game
  | any (\x -> snd (positionInv x) <= -205) invs || any (\a -> recCircIntersect (Rectangle (t1,t2) 36 58) (Circle (positionMis a) 5)) emis =
  game {appstate = Lost}
  | isEmpty invs = startLevel Running (l + 1) sc (t1, t2) movs emis
  | otherwise = game {spaceship = moveSpaceship game, invaders = fst (moveInvaders invs mis), movements = movs, missiles = moveMissiles mis invs, invadersMissiles = moveInvadersMissiles game, randomGenerator = getNewGenerator invs g chance, score = sc + snd (moveInvaders invs mis)}

-- Moves spaceship left or right
moveSpaceship :: Game -> Spaceship
moveSpaceship g = moveSpaceshipLeft $ moveSpaceshipRight g

--Move spaceship left
moveSpaceshipLeft :: Game -> Spaceship
moveSpaceshipLeft game@(Game _ (Spaceship (x,y)) invs movs _ _ _ _ _ _ _) = if PushLeft `elem` movs then
                                                                      if (x - 5) < -455 then
                                                                        Spaceship (-455,y)
                                                                      else Spaceship(x-5,y)
                                                                    else Spaceship (x,y)

-- Move spaceship right, returns game for functions to fold
moveSpaceshipRight :: Game -> Game
moveSpaceshipRight game@(Game state (Spaceship (x,y)) invs movs mis emis g c s p l) = if PushRight `elem` movs then
                                                                            if (x + 5) > 455 then
                                                                              Game state (Spaceship (455,y)) invs movs mis emis g c s p l
                                                                            else Game state (Spaceship (x+5,y)) invs movs mis emis g c s p l
                                                                          else game

-- Move single invader
moveInvader :: Invader -> Invader
moveInvader (Invader (x,y) s dir)
                | x - 1 < s - 10 = Invader (x + 0.5, y - 4) s RightDir
                | x + 1 > s + 10 = Invader (x - 0.5, y - 4) s LeftDir
                | dir == LeftDir = Invader (x - 0.5, y) s LeftDir
                | dir == RightDir = Invader (x + 0.5, y) s RightDir
                | otherwise = error "Something went wrong"

-- Move invaders, delete if spaceship missile reached any, return new list of invaders and score
moveInvaders :: Invaders -> Missiles -> (Invaders, Int)
moveInvaders invs mis = first
  (moveInvader <$>)
  (removeIf
     (\ x
        -> any
             (\ y
                -> recCircIntersect
                     (Rectangle (positionInv x) 68 82) (Circle (positionMis y) 5))
             mis)
     invs)

-- Move single spaceship missile, return new missile
moveMissile :: Missile -> Missile
moveMissile (Missile (x,y) v r) = Missile (x, y + v) v r

-- Move spaceship missiles, delete if any are out of window or were used to kill invader
moveMissiles :: Missiles -> Invaders -> Missiles
moveMissiles mis invs = fst $ removeIf (\x -> snd (positionMis x) > 330) (moveMissile <$> fst (removeIf (\x -> any (\y -> recCircIntersect (Rectangle (positionInv y) 68 82) (Circle (positionMis x) 5)) invs) mis))

-- Move single invader missile
moveInvadersMissile :: Missile -> Missile
moveInvadersMissile (Missile (x,y) v r) = Missile (x, y - v) v r

-- Move invader missiles, delete if any are out of the window are destroyed spaceship
moveInvadersMissiles :: Game -> InvadersMissiles
moveInvadersMissiles game@(Game _ (Spaceship(x,y)) invs _ _ emis g chance _ _ _) = fst $ removeIf (\x -> snd (positionMis x) < -330) ((moveInvadersMissile <$> fst (removeIf (\a -> recCircIntersect (Rectangle (x, y) 36 58) (Circle (positionMis a) 5)) emis)) ++ getNewInvadersMissiles invs g chance)

-- Get new missiles after creating them
getNewInvadersMissiles :: Invaders -> StdGen -> Float -> InvadersMissiles
getNewInvadersMissiles invs g chance = fst (generateInvadersMissilesGenerator invs g chance)

-- Get new generator after creating invader missiles
getNewGenerator :: Invaders -> StdGen -> Float -> StdGen
getNewGenerator invs g chance = snd (generateInvadersMissilesGenerator invs g chance)

-- Given list of invaders, random generator and chance return list of generated missiles and new generator
generateInvadersMissilesGenerator :: Invaders -> StdGen -> Float -> (InvadersMissiles, StdGen)
generateInvadersMissilesGenerator [] g chance = ([], g)
generateInvadersMissilesGenerator (x:xs) g chance
  | null xs = ([Missile (positionInv x) 10 5 | fst (pseudoProbabilisticEvent chance g)], snd (pseudoProbabilisticEvent chance g))
  | otherwise = first
  (
     ([Missile (positionInv x) 10 5 |
         fst (pseudoProbabilisticEvent chance g)]) ++)
  (generateInvadersMissilesGenerator
     xs (snd (pseudoProbabilisticEvent chance g)) chance)

----------HELPER FUNCTIONS----------
-- Remove all given elements from list and return new list
removeItem :: Eq a => a -> [a] -> [a]
removeItem _ []     = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys

-- Remove all elements that fulfill given boolean expresion, return new list and number of deleted elements
removeIf :: (a -> Bool) -> [a] -> ([a], Int)
removeIf _ []     = ([], 0)
removeIf cond (y:ys)  | cond y    = (fst(removeIf cond ys), snd(removeIf cond ys) + 1)
                      | otherwise = first (y :) (removeIf cond ys)

-- Checks if Rectangle and Circle are intersecting (works only if rectangle is alligned with axis)
recCircIntersect :: Rectangle -> Circle -> Bool
recCircIntersect (Rectangle (x, y) w h) (Circle o r)
  | circleDistanceX > (w/2 + r) = False
  | circleDistanceY > (h/2 + r) = False
  | circleDistanceX <= w/2 = True
  | circleDistanceY <= h/2 = True
  | otherwise = cornerDistance <= r ** 2
  where
    circleDistanceX = abs (fst o - x)
    circleDistanceY = abs (snd o - y)
    cornerDistance = (circleDistanceX - w/2) ** 2 + (circleDistanceY - h/2) ** 2

-- With given probability of success and random generator returns Bool and new generator
pseudoProbabilisticEvent :: Float -> StdGen -> (Bool, StdGen)
pseudoProbabilisticEvent chance g = (fst (uniformR (1, 100000) g) <= (chance * 100), snd (uniformR (1 :: Float, 100000 :: Float) g))

-- Check if list is empty
isEmpty :: [a] -> Bool
isEmpty myList
  = case myList of
      [] -> True
      _ -> False
