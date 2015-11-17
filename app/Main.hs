{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings, DeriveGeneric #-}
module Main where

--import Lib
--import Game

--main :: IO ()
--main = do
--    let game = initGame "hello" "world"
--    let pg = (\g -> printGame g)
--    game >>= pg
--    let game' = game >>= (\g -> setReady g "hello") >>= (\g -> setReady g "world")
--    game' >>= pg
--    let game'' = game' >>= tick >>= tick >>= tick
--    game'' >>= pg
--    someFunc

import Yesod.Core
import Yesod.WebSockets
import Conduit
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)
import Data.Aeson
import Data.IORef
import Data.Maybe
import Data.Monoid ((<>))
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Time
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
            -- update database maybe in a different thread that kills itself
            setCompleted g
        else return g

updateGames :: TVar [IO Game] -> IO ()
updateGames games = forever $ do
    atomically $ modifyTVar games (\gs -> map (\iog -> iog >>= updateGame) gs)
    threadDelay _GAME_TICK_DELAY

createGame :: Pong -> String -> String -> IO ()
createGame app lPlayer rPlayer = do
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

--leaveGame :: String -> Map String Int -> IO ()
--leaveGame player userToGameId =
--    delete player userToGameId

msgSource :: MonadIO m => IORef (Maybe (TChan Message)) -> TChan Message -> Source m ByteString
msgSource gameChan chatChan = forever $ do
    maybeGameChan <- liftIO $ readIORef gameChan
    let isInGame = not $ isNothing maybeGameChan
    msg <- liftIO $ if isInGame
        then atomically $ (readTChan $ fromJust maybeGameChan) `orElse` (readTChan chatChan)
        else atomically $ readTChan chatChan
    yield $ toStrict $ encode $ msgData msg

appHandler :: WebSocketsT Handler ()
appHandler = do
    sess <- getSession
    app <- getYesod
    gameChannel <- liftIO $ newIORef Nothing
    chatChannel <- liftIO $ atomically $ dupTChan (chatChan app)
    let msgs = msgSource gameChannel chatChannel
    race_
        (msgs $$ sinkWSText)
        (sourceWS $$ mapC TL.toUpper =$ sinkWSText)
        --(sourceWS $$ mapM_C (\msg -> do liftIO $ print msg))
            --atomically $ writeTChan chatChannel $ "hello" <> ": " <> msg)) -- send message

    --sess <- getSession
    --app <- getYesod
    --gameChannel <- newIORef Nothing
    --chatChannel <- atomically $ dupTChan (chatChan app)
    --let writeChatChan = chatChan app
    --race_
    --    (forever $ do
            --msg <- getMsgFromChans gameChannel chatChannel
            --if ((msgType msg) == ChallengeMsg &&
            --    (challenged (msgData msg)) /= (lookupSession "username"))
            --    then return
            --    else (encode $ msg) >>= sendTextData
    --        -- if it's a challenge message not directed toward user ignore
    --        -- otherwise let chat continue
    --        --if ((msgType msg) == ChallengeMsg &&
    --        --    (challenged (msgData msg)) != (lookupSession "username"))
    --        --   then return
    --        --   else (encode $ toJSON msg) >>= sendTextData
    --    )
    --    (sourceWS $$ mapM_C (\msg ->
    --        atomically $ writeTChan writeChatChan $ ": " <> msg))

--getMsgFromChans :: IORef (Maybe (TChan Message)) -> TChan Message -> IO (Data.ByteString.ByteString)
--getMsgFromChans gameChan chatChan = do
--    maybeGameChan <- readIORef gameChan
--    let isInGame = not $ isNothing maybeGameChan
--    msg <- if isInGame
--        then atomically $ (readTChan $ fromJust maybeGameChan) `orElse` (readTChan chatChan)
--        else atomically $ readTChan chatChan
--    return $ encode $ msgData msg

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
