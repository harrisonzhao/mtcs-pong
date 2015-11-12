module Foundation where

import Yesod.Core
import Yesod.WebSockets
import Conduit
import Control.Monad
import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Concurrent
import Data.Aeson
import Data.IORef
import Data.Maybe
import Data.Monoid ((<>))
--import Data.Map

import Messages
import Game

data Pong = Pong {
    --userToGameId :: Map String Int,
    --challengedToChallenger :: Map String String,
    games :: TVar [IO Game],
    chatChan :: TChan Message,
    nextGameId :: TVar Int
    -- usersOnline :: TVar Set String
}

instance Yesod Pong

_GAME_TICK_DELAY = 1000000

updateGame :: Game -> IO Game
updateGame game = (tick game) >>= \g -> do
    if needsLogging g
        then do
            let (winner, loser) = getWinLose g
            -- update database maybe in a different thread that kills itself
            setCompleted g
        else return g

updateGames :: TVar [IO Game] -> IO ()
updateGames games = forever $ do
    atomically $ modifyTVar games (\gs -> map (\iog -> iog >>= updateGame) games)
    threadDelay _GAME_TICK_DELAY

createGame :: Pong -> String -> String -> IO ()
createGame app lPlayer rPlayer = do
    atomically $ do
        let gameId = readTVarIO (nextGameId app)
        modifyTVar (nextGameId app) (\n -> n + 1)
        modifyTVar (games app) (\gs -> gs ++ (initGame lPlayer rPlayer))
    --insert lPlayer gameId (userToGameId app)
    --insert rPlayer gameId (userToGameId app)
        --writeTChan (chatChan app) for lPlayer
        --writeTChan (chatChan app) for rPlayer

--getGame :: Int -> TVar [IO Game] -> Maybe IO Game
getGame id games = do
    let gameList = readTVarIO games
    if id < (length gameList)
        then return Just gameList !! id
        else return Nothing

--leaveGame :: String -> Map String Int -> IO ()
--leaveGame player userToGameId =
--    delete player userToGameId

--appHandler :: WebSocketsT Handler ()
appHandler = do
    sess <- getSession
    app <- getYesod
    gameChannel <- newIORef Nothing
    chatChannel <- atomically $ dupTChan (chatChan app)
    let writeChatChan = chatChan app
    race_
        (forever $ do
            let msg = getMsgFromChans gameChannel chatChannel
            (encode $ msg) >>= sendTextData
            -- if it's a challenge message not directed toward user ignore
            -- otherwise let chat continue
            --if ((msgType msg) == ChallengeMsg &&
            --    (challenged (msgData msg)) != (lookupSession "username"))
            --   then return
            --   else (encode $ toJSON msg) >>= sendTextData
        )
        (sourceWS $$ mapM_C (\msg ->
            atomically $ writeTChan writeChatChan $ ": " <> msg))

--getMsgFromChans :: IORef Maybe TChan Message -> TChan Message -> IO (Message)
getMsgFromChans gameChan chatChan = do
    let isInGame = (readIORef gameChan) >>= (\gc -> isNothing gc)
    let msg = if isInGame
        then atomically $ readTChan gameChan `orElse` readTChan chatChan
        else atomically $ readTChan chatChan
    return msg

--getHomeR :: Handler Html
--getHomeR = do
--    webSockets appHandler

{-
dup the chatChannel

forever
a) read from chat channel or from game channel (if there is a game channel)
    -- if there is a challenge to the user send it through otherwise filter it out
b) read from the websocket
    -- process message 
-}



--General Flow of Game Updates
{-

need to filter outgoing messages by user string, simple compare if directed message or not
(challenge and initial game if challenge accepted)

createGame :: lPlayer -> rPlayer -> IO ()
    creates game and puts it in games list
    sends a message to both players along with the game

leaveGame :: IO ()

daemon for handling game ticks
    pass around the TVar for games

user registration login logout

handlers for:
    chat
    challenge (first word of chat should be /challenge)
    accept challenge
    get rating
    move paddle
    handle ready

-}