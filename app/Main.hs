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
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text, isPrefixOf, splitOn, unpack, pack, concat)
import Data.String 
import qualified Data.Text as L

import Messages
import Game 

data Pong = Pong {
    games :: TVar [IO Game],
    nextGameId :: TVar Int,
    --chatChan can send chat and challenges
    chatChan :: TChan Message,
    --userToGameId maps users to an index for games list
    userToGameId :: TVar (Map.Map String Int),
    --map for challenged to challenger, modified whenever a challenge is sent or accepted
    challengedToChallenger :: TVar (Map.Map String String),
    --mapping for users online, checked if send challenge
    usersOnline :: TVar (Set.Set String)
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
    atomically $ do
        currentGameId <- readTVar (nextGameId app)
        modifyTVar (nextGameId app) (\n -> n + 1)
        modifyTVar (games app) (\gs -> gs ++ [(initGame lPlayer rPlayer)])
        modifyTVar (userToGameId app) (\mapping -> Map.insert lPlayer currentGameId mapping)
        modifyTVar (userToGameId app) (\mapping -> Map.insert rPlayer currentGameId mapping)

-- call this function to make a given user join a game
-- it duplicates the games channel into the user's game channel IORef
-- gameChannel is the user's gameChannel IORef, which is created in appHandler
joinGame :: String -> IORef (Maybe (TChan Message)) -> Pong -> IO ()
joinGame username gameCh app = do
    gamesLength <- readTVarIO (nextGameId app)
    userGameIdMap <- readTVarIO (userToGameId app)
    let gameId = Map.lookup username userGameIdMap
    if (isJust gameId)
        then do
            let game = getGame (fromJust gameId) gamesLength (games app)
            if (isJust game)
                then do
                    ch <- (fromJust game) >>= (\g -> return (gameChan g))
                    duppedCh <- atomically $ dupTChan ch
                    modifyIORef gameCh (\_ -> Just duppedCh)
                else return ()
        else return ()

-- leaves the game by clearing the game channel IORef
leaveGame :: IORef (Maybe (TChan Message)) -> IO ()
leaveGame gameChannel = do
    modifyIORef gameChannel (\_ -> Nothing)

getGame :: Int -> Int -> TVar [IO Game] -> Maybe (IO Game)
getGame id gamesLength games = do
    if id < gamesLength
        then Just $ (readTVarIO games) >>= (\games -> games !! id) 
        else Nothing

-- Initialized by passing in the three parameters,
-- each accepting connection has a different copy
-- Continuously spits out messages to be sent to client
-- Either:
--     spits out a message formatted as {msgType: String, msgData: json}
--     or empty string (if it's a challenge message not directed toward the user)
-- see src/Messages.hs for all the different message types
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

handleLogin app name = do 
    liftIO $ atomically $ modifyTVar (usersOnline app) (\s -> Set.insert name s)
    
handleChat app msg = do 
    liftIO $ atomically $ writeTChan (chatChan app) $ newChatMsg msg

handleMove app player direction = do
    let pid = unpack player 
    myMap <- liftIO $ readTVarIO (userToGameId app)
    let gameId = Map.lookup pid myMap
    if isJust gameId
        then do
            gamesLength <- liftIO $ readTVarIO (nextGameId app)
            game <- liftIO $ fromJust $ getGame (fromJust gameId) gamesLength (games app)
            case direction of
                "up" -> liftIO $ Game.movePaddle game pid (1)
                "down" -> liftIO $ Game.movePaddle game pid (-1)
            liftIO $ atomically $ writeTChan (chatChan app) $ newChatMsg "move"
        else
            liftIO $ atomically $ writeTChan (chatChan app) $ newChatMsg "move"
    
handleChallenge app player1 player2 = do
    liftIO $ atomically $ modifyTVar (challengedToChallenger app) (\s -> Map.insert (unpack player2) (unpack player1) s)
    liftIO $ atomically $ writeTChan (chatChan app) $ newChatMsg $ L.concat [player1, " ",player2]
    
handleAccept app acceptingPlayer = do
    let acceptingPlayerU = unpack acceptingPlayer
    myMap <- liftIO $ readTVarIO (challengedToChallenger app)
    let challenger = Map.lookup acceptingPlayerU myMap
    currentGameId <- liftIO $ readTVarIO (nextGameId app)
    if isJust challenger
        then do
            liftIO $ atomically $ modifyTVar (challengedToChallenger app) (\s -> Map.delete acceptingPlayerU s) 
            liftIO $ atomically $ modifyTVar (userToGameId app) (\s -> Map.insert acceptingPlayerU currentGameId s) 
            liftIO $ atomically $ modifyTVar (userToGameId app) (\s -> Map.insert (fromJust challenger) currentGameId s) 
            liftIO $ createGame app (fromJust challenger) acceptingPlayerU
            gameChannel <- liftIO $ newIORef Nothing
            liftIO $ joinGame (fromJust challenger) gameChannel app 
            liftIO $ joinGame acceptingPlayerU gameChannel app 
        else
            liftIO $ atomically $ writeTChan (chatChan app) $ newChatMsg "aceppted a nonexisting challenge"
    
handleLeave app msg = do
    --need to actually implement this
    liftIO $ atomically $ writeTChan (chatChan app) $ newChatMsg "leave"

handleOther app msg = do
    liftIO $ atomically $ writeTChan (chatChan app) $ newChatMsg ""

handleMsg app msg = do
    let msgParsed = splitOn "`" msg
    let msgType = msgParsed !! 0
    case msgType of 
        "login" -> 
            handleLogin app $ unpack $ msgParsed !! 1
        "chat" -> 
            handleChat app msg 
        "move" -> 
            handleMove app (msgParsed !! 1) (msgParsed !! 2) 
        "challenge" -> 
            handleChallenge app (msgParsed !! 1) (msgParsed !! 2) 
        "accept" -> 
            handleAccept app (msgParsed !! 1)
        "leave" -> 
            handleLeave app msg 
        _ -> 
            handleOther app msg
    mySet <- liftIO $ readTVarIO (usersOnline app)
    liftIO $ print $ Set.toList mySet
    myMap <- liftIO $ readTVarIO (userToGameId app)
    liftIO $ print $ Map.toList myMap
    myMap2 <- liftIO $ readTVarIO (challengedToChallenger app)
    liftIO $ print $ Map.toList myMap2

appHandler :: WebSocketsT Handler ()
appHandler = do
    sess <- getSession
    app <- getYesod
    -- gameChannel is an IORef because the underlying gameChannel could change
    gameChannel <- liftIO $ newIORef Nothing
    chatChannel <- liftIO $ atomically $ dupTChan (chatChan app)
    let msgs = msgSource "placeholder username" gameChannel chatChannel
    let msgHandler = handleMsg app
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
    games <- newTVarIO ([] :: [IO Game])
    nextGameId <- newTVarIO (0 :: Int)
    chatChan <- atomically newBroadcastTChan
    userToGameId <- newTVarIO (Map.empty)
    challengedToChallenger <- newTVarIO (Map.empty)
    usersOnline <- newTVarIO (Set.empty)
    warp 3000 $ Pong games nextGameId chatChan userToGameId challengedToChallenger usersOnline

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
