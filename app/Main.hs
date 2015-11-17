{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings, DeriveGeneric #-}
module Main where

import Yesod.Core
import Yesod.WebSockets
import Conduit
import Control.Monad
import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)
import Data.Aeson
import Data.IORef
import Data.Maybe
import Data.Monoid ((<>))
import Data.ByteString (ByteString, empty)
import Data.ByteString.Lazy (toStrict)
import qualified Data.Text.Lazy as TL
--import Data.Map

import Messages
import Game

data Pong = Pong {
    --userToGameId :: Map String Int,
    --challengedToChallenger :: Map String String,
    games :: TVar [IO Game],
    nextGameId :: TVar Int,
    chatChan :: TChan Message
    -- usersOnline :: TVar Set String
}

instance Yesod Pong

mkYesod "Pong" [parseRoutes|
/ HomeR GET
|]

_GAME_TICK_DELAY = 1000000

updateGame :: Game -> IO Game
updateGame game = (tick game) >>= \g -> do
    if needsLogging g
        then do
            let maybeWinLosePair = getWinLose g
            -- maybeWinLosePair is Maybe (a, b) where (a, b) is a tuple
            -- update database maybe in a different thread that kills itself
            -- maybe pass along the gameChan so a db updated message can also be seen by users
            -- in order for users to be updated, another message type would be needed
            setCompleted g
        else return g

updateGames :: TVar [IO Game] -> IO ()
updateGames games = forever $ do
    atomically $ modifyTVar games (\gs -> map (\iog -> iog >>= updateGame) gs)
    threadDelay _GAME_TICK_DELAY

createGame :: Pong -> String -> String -> IO ()
createGame app lPlayer rPlayer = do
    -- add player to player to gameId map
    -- remove player from challenged map
    atomically $ do
        modifyTVar (nextGameId app) (\n -> n + 1)
        modifyTVar (games app) (\gs -> gs ++ [(initGame lPlayer rPlayer)])
    --insert lPlayer gameId (userToGameId app)
    --insert rPlayer gameId (userToGameId app)
        --writeTChan (chatChan app) for lPlayer
        --writeTChan (chatChan app) for rPlayer

getGame :: Int -> Int -> TVar [IO Game] -> Maybe (IO Game)
getGame id gameListLength games = do
    if id < gameListLength
        then Just $ (readTVarIO games) >>= (\games -> games !! id) 
        else Nothing

-- Initialized by passing in the three parameters,
-- each accepting connection has a different copy
-- Continuously spits out messages to be sent to client
-- Either:
--     spits out a message formatted as {msgType: String, msgData: json}
--     or empty string (if it's a challenge message not directed toward the user)
msgSource :: MonadIO m => String -> IORef (Maybe (TChan Message)) -> TChan Message -> Source m ByteString
msgSource username gameChan chatChan = forever $ do
    maybeGameChan <- liftIO $ readIORef gameChan
    let isInGame = not $ isNothing maybeGameChan
    msg <- liftIO $ if isInGame
        then atomically $ (readTChan $ fromJust maybeGameChan) `orElse` (readTChan chatChan)
        else atomically $ readTChan chatChan
    if ((msgType msg) == ChallengeMsg)
        then do
            let challenge = decode $ encode $ msgData msg :: Maybe Challenge
            if ((isJust challenge) && ((challenged (fromJust challenge)) == username))
                then yieldMsg msg
                else yield empty
        else yieldMsg msg
  where yieldMsg msg = yield $ toStrict $ encode $ msgData msg

--msg is some web sockets data, which is of Text form
handleMsg chatChannel msg = liftIO $ atomically $ writeTChan chatChannel $ newChatMsg msg

appHandler :: WebSocketsT Handler ()
appHandler = do
    sess <- getSession
    app <- getYesod
    gameChannel <- liftIO $ newIORef Nothing
    chatChannel <- liftIO $ atomically $ dupTChan (chatChan app)
    let msgs = msgSource "placeholder username" gameChannel chatChannel
    let msgHandler = handleMsg chatChannel
    race_
        (msgs $$ sinkWSText)
        (sourceWS $$ mapM_C msgHandler)

getHomeR :: Handler Html
getHomeR = do
    webSockets appHandler
    defaultLayout $ do
        [whamlet|
            <div #output>
            <form #form>
                <input #input autofocus>
        |]
        toWidget [lucius|
            \#output {
                width: 600px;
                height: 400px;
                border: 1px solid black;
                margin-bottom: 1em;
                p {
                    margin: 0 0 0.5em 0;
                    padding: 0 0 0.5em 0;
                    border-bottom: 1px dashed #99aa99;
                }
            }
            \#input {
                width: 600px;
                display: block;
            }
        |]
        toWidget [julius|
            var url = document.URL,
                output = document.getElementById("output"),
                form = document.getElementById("form"),
                input = document.getElementById("input"),
                conn;

            url = url.replace("http:", "ws:").replace("https:", "wss:");
            conn = new WebSocket(url);

            conn.onmessage = function(e) {
                var p = document.createElement("p");
                p.appendChild(document.createTextNode(e.data));
                output.appendChild(p);
            };

            form.addEventListener("submit", function(e){
                conn.send(input.value);
                input.value = "";
                e.preventDefault();
            });
        |]

main :: IO ()
main = do
    chatChan <- atomically newBroadcastTChan
    nextGameId <- newTVarIO (0 :: Int)
    games <- newTVarIO ([] :: [IO Game])
    warp 3000 $ Pong games nextGameId chatChan

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
