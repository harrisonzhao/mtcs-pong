{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Game
    ( Game
    , printGame
    , initGame
    , tick
    , movePaddle
    , getWinLose
    , getPlayers
    , isPlaying
    , setReady
    , needsLogging
    , setCompleted
    , getGameMsg
    , Direction(Up,Down)
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

import Messages

import Data.Aeson
import Data.Maybe
import Data.ByteString
import GHC.Generics
import System.Random

printGame :: Game -> IO ()
printGame game = do
    print "=========== BEGIN GAME PRINT ==========="
    print $ "lPlayer: " ++ (lPlayer game)
    print $ "rPlayer: " ++ (rPlayer game)
    print "status:"
    print (gameStatus game)
    print (state game)
    print "=========== END GAME PRINT ==========="

_WIDTH = 1550
_HEIGHT = 676
_BOTTOM = 0
_LEFT = 0

_INITIAL_BALL_X = _WIDTH / 2
_INITIAL_BALL_Y = _HEIGHT / 2
_INITIAL_BALL_V = 2

_PADDLE_WIDTH = 15
_PADDLE_HEIGHT = 70
_BALL_RADIUS = 1
_MAX_SCORE = 5
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

data Game = Game {
    gameStatus :: Status,
    lPlayer :: String, 
    rPlayer :: String,
    state :: GameState
} deriving (Show, Generic)
instance ToJSON Game

createStatusUpdate :: Game -> StatusUpdate
createStatusUpdate game = StatusUpdate { status = gameStatus game }

initBall :: Int -> Ball
initBall num = do 
    Ball { bx = _INITIAL_BALL_X
         , by = _INITIAL_BALL_Y
         , bvx = fst $ getRandom num
         , bvy = snd $ getRandom num 
         , bradius = _BALL_RADIUS
         }

getRandom num 
    | (mod num 4) == 3 = (2,2)
    | (mod num 4) == 2 = (-2,2)
    | (mod num 4) == 1 = (-2,-2)
    | (mod num 4) == 0 = (2,-2)


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
                then _LEFT + _PADDLE_WIDTH - 15
                else _WIDTH - _PADDLE_WIDTH 


initState :: GameState
initState =
    GameState { ball = initBall 0
              , lPaddle = initPaddle True
              , rPaddle = initPaddle False
              , lPoints = 0
              , rPoints = 0
              , width = _WIDTH
              , height = _HEIGHT
              , maxScore = _MAX_SCORE
              }

initGame :: String -> String -> Game
initGame lPlayerName rPlayerName =
    Game { gameStatus = Initial
         , lPlayer = lPlayerName
         , rPlayer = rPlayerName
         , state = initState
         }


detectCollision :: GameState -> GameState
detectCollision state
    | (y + ballRadius) >= height = state { ball = theBall { by = height - ballRadius, bvy = -vy } }
    | (y - ballRadius) <= 0 = state { ball = theBall { by = ballRadius, bvy = -vy } }
    | (x + ballRadius) >= width = state { ball = initBall (lPoints+rPoints), lPoints = lPoints + 1}
    | (x - ballRadius) <= 0 = state { ball = initBall (lPoints+rPoints), rPoints = rPoints + 1 }
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
            y <= yl + lheight &&
            y >= yl - lheight 
            = -vx
        -- right paddle
        |   x + ballRadius >= xr - rwidth &&
            y <= yr + rheight &&
            y >= yr -rheight 
            = -vx
        | otherwise = vx

tick' :: Game -> Game
tick' game = do 
    let state' = update state
    if lPoints == _MAX_SCORE || rPoints == _MAX_SCORE
        then game { state = state', gameStatus = CompletedNotLogged }
        else game { state = state' }
  where
    Game _ _ _ state = game
    GameState _ _ _ lPoints rPoints _ _ _ = state
    update = moveBall . paddleHit . detectCollision

movePaddle' :: GameState -> Paddle -> Double -> Paddle
movePaddle' state paddle direction =
    paddle { py = y' }
  where
    Paddle x y _ height = paddle
    newY = y + direction * _PADDLE_MOVE
    y' = min (_HEIGHT - height) $ max _BOTTOM newY

{------------------------------------------------------ 
functions for externally seeing and updating game state
------------------------------------------------------}
-- tick should be followed by writing a status update to channel
tick :: Game -> Game
tick game =
    if (gameStatus game) /= Playing
        then game
        else tick' game

getGameMsg :: Game -> Message
getGameMsg game = newMessage GameMsg (toJSON game)

movePaddle :: Game -> String -> Direction -> Game
movePaddle game player direction
    | player == lPlayer = game { state = state {lPaddle = movePaddle' state (lPaddle state) dir} }
    | player == rPlayer = game { state = state {rPaddle = movePaddle' state (rPaddle state) dir} }
    | otherwise = game
  where
    Game _ lPlayer rPlayer state = game
    dir
        | direction == Up = _UP_DIR
        | direction == Down = _DOWN_DIR

getWinLose :: Game -> Maybe (String, String)
getWinLose game
    | lPoints == _MAX_SCORE = Just (lPlayer, rPlayer)
    | rPoints == _MAX_SCORE = Just (rPlayer, lPlayer)
    | otherwise = Nothing
  where
    Game _ lPlayer rPlayer state = game
    GameState _ _ _ lPoints rPoints _ _ _ = state

getPlayers :: Game -> (String, String)
getPlayers game = (lPlayer game, rPlayer game) 

isPlaying :: Game -> Bool 
isPlaying game = (gameStatus game) == Playing 

-- write status update after this
setReady :: Game -> String -> Game
setReady game player =
    game { gameStatus = status' }
  where
    Game status lPlayer rPlayer _ = game
    status'
        | (player == lPlayer) && (status == Initial) = LeftReady
        | (player == lPlayer) && (status == RightReady) = Playing
        | (player == rPlayer) && (status == Initial) = RightReady
        | (player == rPlayer) && (status == LeftReady) = Playing
        | otherwise = status

-- do score logging after this
needsLogging :: Game -> Bool
needsLogging game = (gameStatus game) == CompletedNotLogged

-- write status update after this
setCompleted :: Game -> Game
setCompleted game =
    if (gameStatus game) == CompletedNotLogged
        then game { gameStatus = Completed }
        else game
