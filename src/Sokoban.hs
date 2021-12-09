module Sokoban where

import Data.List

import System.Random

type Point = (Int, Int)

data InitData = InitData {
  hWidth :: Int
  ,hHeight :: Int
  ,manPo :: Point
  ,boxPos :: [Point]
  ,holePos :: [Point]
  ,wallBrickPos :: [Point]
}

inlineSampleInitData :: InitData
inlineSampleInitData = InitData 3 3 (-1,1)
   [(-1,0),(0,0),(1,0),(-1,-1)]  [(0,1),(0,2), (-1,2),(-2,2)] [(-2,0),(1,1),(1,-1)]
gameBoardHalfWidth :: Int
gameBoardHalfWidth = 3


gameBoardHalfHeight :: Int
gameBoardHalfHeight = 9

addBorderProperly :: InitData -> InitData
addBorderProperly o = o { wallBrickPos = oldWPos ++ bounds}
  where 
    oldWPos = wallBrickPos o
    bounds = genBounduaries (hWidth o) $ hHeight o


genBounduaries :: Int -> Int -> [Point]
genBounduaries wid hei = nub $ yBounds ++ xBounds
  where
    yBounds = (foldr1 (++) (map (\f -> map f [(- wid)..wid]) [\x -> (x, (-hei)),\x -> (x,hei)]))
    xBounds = (foldr1 (++) (map (\f -> map f [(- hei)..hei]) [\y -> (wid,y), \y -> ((-wid),y)]))



fixedHolesPositions :: [Point]
fixedHolesPositions = [(-1,1), (-1,2),(-1,3)]

fixedBoxesPositions :: [Point]
fixedBoxesPositions = [(-4,1), (-4,3),(-4,7)]

data GameState = GameReady
 | GameRunning
 | GameFinished
 | GameAborted
 deriving(Show, Eq)


type RandomNumber = Int

data World = World {
 rnds :: [RandomNumber]
 ,allInitGameData :: [InitData]
 ,currentInitGameDataIx :: Int
 ,state :: GameState
 ,man :: Man
 ,boxes :: [Box]
 ,holes :: [Hole]
 ,wallBricks :: [WallBrick]
 ,areaWidth :: Int
 ,areaHeight :: Int
}

data Direction = DirectUp
 | DirectDown
 | DirectLeft
 | DirectRight
 deriving(Show, Eq, Ord)

data Man = Man {
  positionOfMan :: Point
 ,ds :: Double
 ,steps :: Int
 ,direct :: Direction
}

data Box = Box {
  positionOfBox :: Point
 }

data Hole = Hole {
  positionOfHole :: Point
 }


data WallBrick = WallBrick {
  positionOfWallBrick :: Point
 }

initialWorld :: StdGen -> World
initialWorld gen = ww {state = GameReady}
  where
    ww = launchWorld rnds properData rix
    rix = r0 * r0 `mod` (length properData)
    (r0:rnds) = randomRs (0,20) gen
    properData = [addBorderProperly inlineSampleInitData]
    
restartWorld :: World -> World
restartWorld w = launchWorld (rnds w) (allInitGameData w) (currentInitGameDataIx w)

changeNewDataWorld :: World -> World
changeNewDataWorld w = launchWorld rnds' ad rix
  where 
    rix = r0 * r0 `mod` (length ad)
    ad = allInitGameData w
    (r0:rnds') = rnds w
    
launchWorld :: [RandomNumber] -> [InitData] -> Int -> World
launchWorld rnds ad index = World rnds' ad index  GameRunning man boxes holes wallBricks w h
  where 
    (r1:r2:rnds') = rnds 
    man = mkMan $ manPo iData
    boxes = mkBoxes $ boxPos iData
    holes = mkHoles $ holePos iData
    wallBricks = mkWallBricks $ wallBrickPos iData
    mkBoxes ps = map (\p -> mkBox p) ps
    mkHoles ps = map (\p -> mkHole p) ps
    mkWallBricks ps = map(\p -> mkWallBrick p) ps
    w = hWidth iData * 2 + 1
    h = hHeight iData * 2 + 1
    iData = ad !! index




mkMan :: Point -> Man
mkMan positionPoint = Man positionPoint 0 0  DirectUp

mkBox :: Point -> Box 
mkBox positionPoint = Box positionPoint

mkHole :: Point -> Hole 
mkHole positionPoint = Hole positionPoint

mkWallBrick :: Point -> WallBrick
mkWallBrick positionPoint  = WallBrick positionPoint

processTimePassing :: World -> World
processTimePassing w 
 | state w == GameRunning = if judgeFinishedWorld w then w {state = GameFinished } else w
 | otherwise = w
 -- where w' = w {man = m'}
 -- m' = m { colorOfMan = if colorOrig == red then yellow else red}
 -- colorOrig = colorOfMan m
 -- m = man w


judgeFinishedWorld :: World -> Bool
judgeFinishedWorld w = foldr1 (&&) checkList
  where 
    checkList = map (\p -> p `elem` holePos) boxPos
    boxPos = map positionOfBox (boxes w)
    holePos = map positionOfHole (holes w)

processManMoveAction :: Direction -> World -> Man -> World
processManMoveAction dir w mman
 | pushPoint `elem` wallBrickPos = w { man = mman'}
 | pushPoint `elem` boxPos = processManWithBoxMoveAction dir w mman pushPoint
 | otherwise = w { man = mman''}
  where
    mman'' = mman' { positionOfMan = pushPoint, steps = stps + 1}
    stps = steps mman'
    mman' = mman { direct = dir }
    wallBrickPos = map positionOfWallBrick (wallBricks w)
    boxPos = map positionOfBox (boxes w)
    pushPoint = getAdjcentPointOnDirection dir (positionOfMan oldMan)
    oldMan = mman

processManWithBoxMoveAction :: Direction -> World -> Man -> Point -> World
processManWithBoxMoveAction dir w mman boxPo 
 | (pushPushPoint `elem` boxPos) || (pushPushPoint `elem` wallBrickPos) = w { man = mman'}
 | otherwise = w { man = mman'', boxes = boxesOfWorld'}
  where
    boxesOfWorld' =
      map ( \b@(Box p) -> if p == boxPo 
            then b { positionOfBox = pushPushPoint}
            else b
          ) boxesOfWorld
    mman'' = mman' { positionOfMan = boxPo, steps = stps + 1}
    stps = steps mman'
    mman' = mman { direct = dir }
    pushPushPoint = getAdjcentPointOnDirection dir boxPo
    wallBrickPos = map positionOfWallBrick (wallBricks w)
    boxPos = map positionOfBox boxesOfWorld
    boxesOfWorld = boxes w

getAdjcentPointOnDirection :: Direction -> Point -> Point
getAdjcentPointOnDirection dir orig@(x,y)
 | dir == DirectUp = (x, y + 1)
 | dir == DirectDown = (x, y - 1)
 | dir == DirectLeft = (x - 1, y)
 | dir == DirectRight = (x + 1, y)
