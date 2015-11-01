{-# LANGUAGE DeriveGeneric #-}
module Game
    ( Game
    , Status
    , printGame
    , gameChan
    , gameStatus
    , initGame
    , tick
    , movePaddle
    , getWinLose
    , setReady
    , setCompleted
    ) where
{-
User key movements translate to either
    setReady
    movePaddle

General flow of game updates
    tick
    check for CompletedNotLogged games
    if CompletedNotLogged
        call getWinLose & update win/lose in db
        set game to be Completed    
-}

import MessageTypes

import Data.Aeson
import Data.Maybe
import Data.ByteString
import Control.Concurrent.STM
import GHC.Generics

printGame :: Game -> IO ()
printGame game = do
    print "=========== BEGIN GAME ==========="
    print $ "lPlayer: " ++ (lPlayer game)
    print $ "rPlayer: " ++ (rPlayer game)
    print "status:"
    print (gameStatus game)
    print (state game)
    print "=========== END GAME ==========="

_WIDTH = 500
_HEIGHT = 600
_BOTTOM = 0
_LEFT = 0

_INITIAL_BALL_X = _HEIGHT / 2
_INITIAL_BALL_Y = _WIDTH / 2
_INITIAL_BALL_V = 2

_PADDLE_WIDTH = 20
_PADDLE_HEIGHT = 5
_BALL_RADIUS = 5
_MAX_SCORE = 1
_PADDLE_MOVE = 10

_UP_DIR = 1
_DOWN_DIR = -1

-- increasing y is up
-- increasing x is right
data Ball = Ball {
    bx :: Double,
    by :: Double,
    bvx :: Double,
    bvy :: Double,
    bradius :: Double
} deriving (Show, Generic)
instance ToJSON Ball

data Paddle = Paddle {
    px :: Double,
    py :: Double,
    pwidth :: Double,
    pheight :: Double
} deriving (Show, Generic)
instance ToJSON Paddle

data Status = Initial 
            | LeftReady
            | RightReady
            | Playing
            | CompletedNotLogged
            | Completed 
            deriving (Eq, Show, Generic)
instance ToJSON Status

data StatusUpdate = StatusUpdate {
  status :: Status
} deriving (Show, Generic)
instance ToJSON StatusUpdate

data Direction = Up | Down deriving Eq

data GameState = GameState {
    ball :: Ball,
    lPaddle, rPaddle :: Paddle,
    lPoints, rPoints :: Int,
    width :: Double,
    height :: Double,
    maxScore :: Int
} deriving (Show, Generic)
instance ToJSON GameState

-- dataChan and dbChan are write only channels
data Game = Game {
    gameStatus :: Status,
    lPlayer :: String, 
    rPlayer :: String,
    state :: GameState,
    gameChan :: TChan (MessageType, Value)
}

createStatusUpdate :: Game -> StatusUpdate
createStatusUpdate game =
    StatusUpdate { status = gameStatus game }

initBall :: Ball
initBall =
    Ball { bx = _INITIAL_BALL_X
         , by = _INITIAL_BALL_Y
         , bvx = _INITIAL_BALL_V
         , bvy = _INITIAL_BALL_V
         , bradius = _BALL_RADIUS
         }

-- Paddle x, y denote lower left corner
initPaddle :: Bool -> Paddle
initPaddle isLeft =
    Paddle { px = xPos
           , py = (_HEIGHT + _BOTTOM)/2
           , pwidth = _PADDLE_WIDTH
           , pheight = _PADDLE_HEIGHT
           }
        where
            xPos = if isLeft
                then _LEFT + _PADDLE_HEIGHT
                else _WIDTH - _PADDLE_HEIGHT

initState :: GameState
initState =
    GameState { ball = initBall
              , lPaddle = initPaddle True
              , rPaddle = initPaddle False
              , lPoints = 0
              , rPoints = 0
              , width = _WIDTH
              , height = _HEIGHT
              , maxScore = _MAX_SCORE
              }

initGame :: String -> String -> IO Game
initGame lPlayerName rPlayerName = do
    gameChan <- newBroadcastTChanIO
    return Game { gameStatus = Initial
                , lPlayer = lPlayerName
                , rPlayer = rPlayerName
                , state = initState
                , gameChan = gameChan
                }

detectCollision :: GameState -> GameState
detectCollision state
    | (y + ballRadius) >= height = state { ball = theBall { by = height - ballRadius, bvy = -vy } }
    | (y - ballRadius) <= 0 = state { ball = theBall { by = ballRadius, bvy = -vy } }
    | (x + ballRadius) >= width = state { ball = initBall, lPoints = lPoints + 1}
    | (x - ballRadius) <= 0 = state { ball = initBall, rPoints = rPoints + 1 }
    | otherwise = state
  where
    GameState theBall _ _ lPoints rPoints width height _ = state
    Ball x y vx vy ballRadius = theBall

moveBall :: GameState -> GameState
moveBall state = state { ball = theBall { bx = x + vx, by = y + vy }}
  where
    theBall = ball state
    Ball x y vx vy _ = theBall

paddleHit :: GameState -> GameState
paddleHit state =
    state { ball = theBall { bvx = vx' } }
  where
    theBall = ball state
    Ball x y vx vy ballRadius = theBall
    Paddle xl yl lwidth lheight = lPaddle state
    Paddle xr yr rwidth rheight = rPaddle state
    vx'
        -- left paddle
        |   x - ballRadius <= xl + lwidth &&
            y + ballRadius >= yl &&
            y <= yl + lheight
            = -vx
        -- right paddle
        |   x + ballRadius >= xr &&
            y + ballRadius >= yr &&
            y <= yr + rheight
            = -vx
        | otherwise = vx

writeStatusUpdate :: Game -> IO ()
writeStatusUpdate game =
    atomically $ writeTChan (gameChan game) $ (GameStatusMsg, toJSON $ createStatusUpdate game)

{- 
functions for externally updating game state
-}
tick :: Game -> IO Game
tick game = do
    if (gameStatus game) /= Playing
        then return game
        else tick' game

tick' :: Game -> IO Game
tick' game = do 
    let state' = update state
    if lPoints == _MAX_SCORE || rPoints == _MAX_SCORE
        then do
            let game = game { gameStatus = CompletedNotLogged }
            writeStatusUpdate game
            return game
        else do
            atomically $ writeTChan gameChan $ (GameStateMsg, toJSON state')
            return game { state = state' }
  where
    Game _ _ _ state gameChan = game
    GameState _ _ _ lPoints rPoints _ _ _ = state
    update = paddleHit . moveBall . detectCollision

movePaddle :: Game -> String -> Direction -> IO Game
movePaddle game player direction
    | player == lPlayer = return game { state = state {lPaddle = movePaddle' state (lPaddle state) dir} }
    | player == rPlayer = return game { state = state {rPaddle = movePaddle' state (rPaddle state) dir} }
    | otherwise = return game
  where
    Game _ lPlayer rPlayer state _ = game
    dir
        | direction == Up = _UP_DIR
        | direction == Down = _DOWN_DIR

movePaddle' :: GameState -> Paddle -> Double -> Paddle
movePaddle' state paddle direction =
    paddle { py = y' }
  where
    Paddle x y _ height = paddle
    newY = y + direction * _PADDLE_MOVE
    y' = min (_HEIGHT - height) $ max _BOTTOM newY

getWinLose :: Game -> Maybe (String, String)
getWinLose game
    | lPoints == _MAX_SCORE = Just (lPlayer, rPlayer)
    | rPoints == _MAX_SCORE = Just (rPlayer, lPlayer)
    | otherwise = Nothing
  where
    Game _ lPlayer rPlayer state _ = game
    GameState _ _ _ lPoints rPoints _ _ _ = state

setReady :: Game -> String -> IO Game
setReady game player =
    return game { gameStatus = status' }
  where
    Game status lPlayer rPlayer _ _ = game
    status'
        | (player == lPlayer) && (status == Initial) = LeftReady
        | (player == lPlayer) && (status == RightReady) = Playing
        | (player == rPlayer) && (status == Initial) = RightReady
        | (player == rPlayer) && (status == LeftReady) = Playing
        | otherwise = status

setCompleted :: Game -> IO Game
setCompleted game = do
    let status = gameStatus game
    if status == CompletedNotLogged
        then return game { gameStatus = Completed }
        else return game
