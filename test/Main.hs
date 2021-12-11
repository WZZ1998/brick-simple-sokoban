import Test.Framework
import Test.HUnit
import Test.Framework.Providers.HUnit
import Paths_brick_simple_sokoban
import Sokoban
import Model
main :: IO ()
main = do
    w <- readFilCreateWorld
    defaultMain [
         testCase "test init world state" $ initWTest w
        ,testCase "test restart world state" $ restartWTest w
        ,testCase "test change index of world" $ changeWTest w
        ,testCase "test tick world" $ tickWTest w
        ,testCase "test judge finish world" $ judgeFinishWTest w
        ,testCase "test try man move in world" $ judgeManMoveTest w
        ,testCase "test judge box movable" $ boxJudgeTest w
        ,testCase "test update one in boxes" $ boxUpdateTest w
        ,testCase "test translate point on direction" $ directPointTest w
        ]

readFilCreateWorld :: IO World
readFilCreateWorld = do
  -- load init data, create world
  realFileName <- getDataFileName "initData.txt"
  initDataStr <- readFile realFileName
  let legalData = map addBorderProperly $ rInitData initDataStr
  return $ initialWorld legalData



initWTest :: World -> Assertion
initWTest w = assertBool "wrong state of init world" $ state w == GameReady

restartWTest :: World -> Assertion
restartWTest w = assertBool "wrong state after restart world" 
    $ GameRunning == state (restartWorld w)

changeWTest :: World -> Assertion
changeWTest w = assertBool "wrong index after change world"
    $ 0 == currentInitGameDataIx (changeToIndexWorld 0 w)

tickWTest :: World -> Assertion
tickWTest w = assertBool "tick is mul-function for world."
    $ 0 == runningTicks (processTick w)

judgeFinishWTest :: World -> Assertion
judgeFinishWTest w = assertBool "judge finishin game in wrong way."
    $ False == judgeGameFinished w

judgeManMoveTest :: World -> Assertion
judgeManMoveTest w = assertBool "man move direction not turned."
    $ DirectDown == direct (man (processManTryMove DirectDown w))

boxJudgeTest, boxUpdateTest, directPointTest :: World -> Assertion
boxJudgeTest w = assertBool "box judge wrong."
    $ True == isBoxMovableOnDir DirectDown (1,0) w

boxUpdateTest w = assertBool "box update wrong."
    $ (1,1) `elem` ( map positionOfBox (updateOneInBoxes (boxes w) (1,0) (1,1)))

directPointTest w = assertBool "translate point wrong."
    $ (1,0) == getAdjPtOnDir DirectRight (0,0)