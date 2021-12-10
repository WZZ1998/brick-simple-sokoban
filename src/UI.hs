{-# LANGUAGE OverloadedStrings #-}
module UI where

import Sokoban
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor
  , continue, halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, padLeftRight, padTopBottom, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>)
  , (<=>)
  )
import Data.Maybe(fromJust)
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Control.Lens ((^.))
import qualified Graphics.Vty as V
import Data.Sequence (Seq)
import Data.List
import qualified Data.Sequence as S
import System.Random

data Tick = Tick
type Name = ()


app :: App World Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

runMyApplication :: IO()
runMyApplication = do
  gen <- getStdGen
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 250000 -- decides how fast world moves , micro-second
  let w = initialWorld gen
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder 
  void $ customMain initialVty builder (Just chan) app w

-- Handling events

handleEvent :: World -> BrickEvent Name Tick -> EventM Name (Next World)
handleEvent w (AppEvent Tick)                       = continue $ processTimePassing w

handleEvent w (VtyEvent (V.EvKey (V.KChar 'w') []))
  | state w == GameRunning = continue $ processManMoveAction DirectUp w mman
  | otherwise = continue w
    where mman = man w
handleEvent w (VtyEvent (V.EvKey (V.KChar 'a') []))
  | state w == GameRunning = continue $ processManMoveAction DirectLeft w mman
  | otherwise = continue w
    where mman = man w
handleEvent w (VtyEvent (V.EvKey (V.KChar 's') [])) 
  | state w == GameRunning = continue $ processManMoveAction DirectDown w mman
  | otherwise = continue w
    where mman = man w
handleEvent w (VtyEvent (V.EvKey (V.KChar 'd') []))
  | state w == GameRunning = continue $ processManMoveAction DirectRight w mman
  | otherwise = continue w
    where mman = man w

handleEvent w (VtyEvent (V.EvKey (V.KChar 'j') []))
  | state w == GameSelecting = 
        if cix > 0 then continue $ changeNewDataWorld (cix - 1) w else continue w
  | otherwise = continue w
  where 
    cix = currentInitGameDataIx w

handleEvent w (VtyEvent (V.EvKey (V.KChar 'k') []))
  | state w == GameSelecting = 
        if cix < (dLen - 1) then continue $ changeNewDataWorld (cix + 1) w else continue w
  | otherwise = continue w
  where 
    dLen = length (allInitGameData w)
    cix = currentInitGameDataIx w

handleEvent w (VtyEvent (V.EvKey V.KEnter []))
  | state w == GameReady = continue $ changeNewDataWorld 0 w
  | state w == 
    GameFinished || state w == GameAborted || state w == GameSelecting = 
      continue 
      $ restartWorld w
  | otherwise = continue w
    where areaW = areaWidth w
          areaH = areaHeight w
handleEvent w (VtyEvent (V.EvKey (V.KChar 'q') []))
  | state w == GameReady = halt w
  | state w == GameRunning = continue $ w {state = GameAborted}
  | state w == GameFinished || state w == GameAborted = continue 
                $ changeNewDataWorld (currentInitGameDataIx w) w
  | otherwise = continue $ w{state = GameReady}
handleEvent w (VtyEvent (V.EvKey V.KEsc []))        = halt w
handleEvent w _                                     = continue w


-- Drawing

drawUI :: World -> [Widget Name]
drawUI w = case state w of
  GameReady -> drawWelcome w
  _ ->[ C.center $ padTop (Pad 0) (drawStats w) <+> (padLeft (Pad 4) $ drawGrid w )]


drawWelcome :: World -> [Widget Name]
drawWelcome w = [ C.center $ vBox [C.hCenter welcomePaint, padTop (Pad 3) (welcomeText1 <=> welcomeText2)] ]
  where 
    welcomeText1 = C.hCenter $ hLimit (34 * 2) $ str "Welcome to Sokoban!"
    welcomeText2 = C.hCenter $ hLimit (34 * 2) $ str "press <enter> for new game | <q> to exit."
    welcomePaint = hBox (map dummyDraw "s o k o b a n" )
drawStats :: World -> Widget Name
drawStats w = hLimit 25 $ vBox usingStats
  where usingStats
          | state w == GameSelecting = normalStats
          | otherwise  = normalStats ++ runningStats
        normalStats = [ padAll (3) $ drawSlogan  (state w) (runningTicks w), drawLevel $ currentLevel w]
        runningStats = [drawSteps ( steps $ man w) ,drawTime $ runningTicks w]
-- helper

drawManPosition :: Point -> Widget Name
drawManPosition p = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "pos")
  $ C.hCenter
  --  $ padTopBottom 1
  $ padLeftRight 2
  $ str $ show p

drawLevel :: String -> Widget Name
drawLevel l = withBorderStyle BS.unicodeRounded
  $ B.borderWithLabel (str "Level")
  $ C.hCenter
  --  $ padTopBottom 1
  $ padLeftRight 2
  $ paintOnLevel l
  where paintOnLevel l = case l of
          "Medium" -> withAttr mediumLvAttr $ str l
          "Easy" -> withAttr easyLvAttr $ str l
          _ -> withAttr hardLvAttr $ str l

drawSteps :: Int -> Widget Name
drawSteps n = withBorderStyle BS.unicodeRounded
  $ B.borderWithLabel (str "Steps")
  $ C.hCenter
 --  $ padTopBottom 1
  $ padLeftRight 2
  $ str $ show n

drawTime :: Int -> Widget Name
drawTime n = withBorderStyle BS.unicodeRounded
  $ B.borderWithLabel (str "Time (s)")
  $ C.hCenter
--  $ padTopBottom 1
  $ padLeftRight 2
  $ str $ show (n `div` 4)

drawSlogan ::  GameState -> Int -> Widget Name
drawSlogan st tk
  | st == GameAborted = withAttr gameAbortedAttr $ C.hCenter $ str "GAME ABORT"
  | st == GameFinished = withAttr gameFinishedAttr $ C.hCenter $ str "GAME FINISH"
  | st == GameSelecting = withAttr gameSelectingAttr $ C.hCenter $ str
        $ if (tk `div` 4 `mod` 2) == 0 then "SELECTING..." else "        "
  | st == GameRunning = withAttr gameRunningAttr $ C.hCenter $ str 
        $ if (tk `div` 2 `mod` 2) == 0 then "Sokoban!" else "        "
  | otherwise = emptyWidget
    


drawGrid :: World -> Widget Name
drawGrid w = withBorderStyle BS.unicodeRounded
  $ B.borderWithLabel (str $ "Stage: " ++ showN )
  $ vBox rows
  where
    showN = if stageN < 10 then "0" ++ show stageN else show stageN
    stageN = currentInitGameDataIx w
    rows         = [hBox $ cellsInRow r | r <- reverse [(- halfH)..halfH]]
    cellsInRow y = [drawPoint (x, y) | x <- [(-halfW)..halfW]]
    drawPoint    =  drawPointFromWorld w
    halfH = (areaHeight w) `div` 2
    halfW = (areaWidth w) `div` 2

drawPointFromWorld :: World -> Point -> Widget Name
drawPointFromWorld  w p
  | p == manPo = drawMan $ man w
  | p `elem` boxPos && p `elem` holePos = drawBox True $ fromJust $ find (\(Box pb) -> pb == p) bs
  | p `elem` boxPos = drawBox False $ fromJust $ find (\(Box pb) -> pb == p) bs
  | p `elem` holePos = drawHole $ fromJust $ find (\(Hole ph) -> ph == p) hs
  | p `elem` wallBrickPos = drawWallBrick $ fromJust $ find (\(WallBrick pw) -> pw == p) ws
  | otherwise = drawEmpty
    where
      manPo = positionOfMan $ man w
      wallBrickPos = map positionOfWallBrick ws
      holePos = map positionOfHole hs
      boxPos = map positionOfBox bs
      ws = wallBricks w
      hs = holes w
      bs = boxes w

drawMan :: Man -> Widget Name
drawMan man = withAttr manAttr $ case direct man of
  DirectUp ->  manUpSquare
  DirectDown -> manDownSquare
  DirectLeft -> manLeftSquare
  DirectRight -> manRightSquare

drawWallBrick :: WallBrick -> Widget Name
drawWallBrick wallBrick = withAttr wallBrickAttr wallBrickSquare

drawHole :: Hole -> Widget Name
drawHole hole = withAttr holeAttr holeBigSquare

drawBox :: Bool -> Box  -> Widget Name
drawBox ok box = if ok then withAttr okBoxAttr $ boxBigSquare else withAttr boxAttr $ boxBigSquare

drawEmpty :: Widget Name
drawEmpty = withAttr emptyAttr cw

cw :: Widget Name
cw = bigSquare


oneS :: Widget Name
oneS = str "  "

twoSH :: Widget Name
twoSH = hBox [oneS, oneS]

bigSquare :: Widget Name
bigSquare = vBox [twoSH, twoSH]

boxBigSquare :: Widget Name 
boxBigSquare = holeBigSquare

holeBigSquare :: Widget Name 
holeBigSquare = vBox [str " \\/ ", str " /\\ "]

manUpSquare :: Widget Name
manUpSquare = vBox [str " /\\ ", str " || "]

manDownSquare :: Widget Name
manDownSquare = vBox [str " || ", str " \\/ "]

manLeftSquare :: Widget Name
manLeftSquare = vBox [str "/---", str "\\---"]

manRightSquare :: Widget Name
manRightSquare = vBox [str "---\\", str "---/"]


wallBrickSquare :: Widget Name
wallBrickSquare = bigSquare

dummyDraw :: Char -> Widget Name
dummyDraw c = case c of
  's' -> dummyProcess [[0,1,1,1],[1,0,0,0],[0,1,1,0],[0,0,0,1],[1,1,1,0]]
  'o' -> dummyProcess [[1,1,1,1],[1,0,0,1],[1,0,0,1],[1,0,0,1],[1,1,1,1]]
  'k' -> dummyProcess [[1,0,0,1],[1,0,1,0],[1,1,0,0],[1,0,1,0],[1,0,0,1]]
  'b' -> dummyProcess [[1,0,0,0],[1,0,0,0],[1,1,1,1],[1,0,0,1],[1,1,1,1]]
  'a' -> dummyProcess [[1,1,1,1],[1,0,0,1],[1,1,1,1],[1,0,0,1],[1,0,0,1]]
  'n' -> dummyProcess [[1,1,1,1],[1,0,0,1],[1,0,0,1],[1,0,0,1],[1,0,0,1]]
  ' ' -> dummyProcess $ replicate 5 [0]
  _ -> dummyProcess $ replicate 5 [1,1,1,1]


dummyProcess :: [[Int]] -> Widget Name
dummyProcess grid = vBox $ map f grid
  where f arr = hBox $ map (\v -> if v == 1 then withAttr welcomeCharAttr oneS else oneS) arr

boxColor :: V.Color
boxColor = V.rgbColor 247 159 58

okBoxColor :: V.Color
okBoxColor = V.rgbColor 154 64 9

holeColor :: V.Color
holeColor = V.rgbColor 203 130 114

wallBrickColor :: V.Color
wallBrickColor = V.rgbColor 121 111 58
floorColor :: V.Color
floorColor = V.rgbColor 214 206 158
clearRedColor :: V.Color
clearRedColor = V.rgbColor 255 0 0

welcomeCharColor :: V.Color
welcomeCharColor = V.rgbColor 253 126 125

hardLvColor :: V.Color
hardLvColor = V.rgbColor 227 23 13

mediumLvColor :: V.Color
mediumLvColor = V.rgbColor 255 97 3

manColor :: V.Color
manColor = V.rgbColor 0 112 205
theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (manAttr, V.black `on` manColor)
  , (boxAttr, ( V.white `on` boxColor )`V.withStyle` V.bold)
  , (okBoxAttr, ( clearRedColor `on` okBoxColor )`V.withStyle` V.bold)
  , (holeAttr,holeColor `on` floorColor `V.withStyle` V.bold)
  , (wallBrickAttr, V.black `on` wallBrickColor)
  , (gameAbortedAttr, fg V.red `V.withStyle` V.bold)
  , (gameFinishedAttr, fg V.green `V.withStyle` V.bold)
  , (gameSelectingAttr, fg V.blue `V.withStyle` V.bold)
  , (gameRunningAttr, fg boxColor `V.withStyle` V.bold)
  , (emptyAttr, V.white `on` floorColor)
  , (welcomeCharAttr, V.black `on` welcomeCharColor)
  , (hardLvAttr, fg hardLvColor)
  , (easyLvAttr, fg V.green)
  , (mediumLvAttr, fg mediumLvColor)
  ]

gameFinishedAttr :: AttrName
gameFinishedAttr = "gameFinished"

gameAbortedAttr :: AttrName
gameAbortedAttr = "gameAborted"

gameSelectingAttr :: AttrName
gameSelectingAttr = "gameSelecting"

gameRunningAttr :: AttrName
gameRunningAttr = "gameRunning"

manAttr, 
  boxAttr, okBoxAttr, 
  holeAttr, 
  wallBrickAttr, 
  emptyAttr, 
  welcomeCharAttr,
  hardLvAttr,easyLvAttr,mediumLvAttr :: AttrName
manAttr = "manAttr"
holeAttr = "holeAttr"
boxAttr = "boxAttr"
okBoxAttr = "okBoxAttr"
wallBrickAttr = "wallBrickAttr"
emptyAttr = "emptyAttr"
welcomeCharAttr = "welcomCharAttr"
hardLvAttr = "hardLvAttr"
easyLvAttr = "easyLvAttr"
mediumLvAttr = "mediumLvAttr"