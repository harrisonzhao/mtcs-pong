{-# LANGUAGE EmptyDataDecls, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ViewPatterns, TypeSynonymInstances, QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings, DeriveGeneric, RecordWildCards #-}
module Main where

import Yesod.Core
import Yesod.WebSockets
import Yesod.Form
import Yesod.Persist
import Conduit
import Control.Monad
import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Concurrent (threadDelay, forkIO)
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

import Yesod.Form.Jquery
import Data.Text (Text)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Reader
import Control.Lens

--import Control.Monad.IO.Class (liftIO)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.Sql
import Database.Persist.TH

import Messages
import Game

data Pong = Pong {
    persistConfig :: SqliteConf,
    connPool :: ConnectionPool,
    games :: TVar [IO (Game, TChan Message)],
    nextGameId :: TVar Int,
    --chatChan can send chat and challenges
    chatChan :: TChan Message,
    --userToGameId maps users to an index for games list
    userToGameId :: TVar (Map.Map String Int),
    --map for challenged to challenger, modified whenever a challenge is sent or accepted
    challengedToChallenger :: TVar (Map.Map String String),
    --mapping for users online, checked if send challenge
    usersOnline :: TVar (Set.Set String),
    --map for user to IORef to game channel
    userGameChannels :: TVar (Map.Map String (IORef (Maybe (TChan Message))))
}

-- Define our entities as usual
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
 username String
 UsernameKey username
 password String
 gamespl Int
 win Int
 loss Int
    deriving Show
|]

-- Nothing special here
instance Yesod Pong
-- Now we need to define a YesodPersist instance, which will keep track of
-- which backend we're using and how to run an action.

instance YesodJquery Pong

instance RenderMessage Pong FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodPersist Pong where
    type YesodPersistBackend Pong = SqlBackend
    runDB = defaultRunDB persistConfig connPool
--    runDB action = do
--        pongApp <- getYesod
--        runSqlPool action (pool pongApp)
instance YesodPersistRunner Pong where
    getDBRunner = defaultGetDBRunner connPool

-- We keep our connection pool in the foundation. At program initialization, we
-- create our initial pool, and each time we need to perform an action we check
-- out a single connection from the pool.
-- data Pong = Pong ConnectionPool

-- We'll create a single route, to access a person. It's a very common
-- occurrence to use an Id type in routes.
mkYesod "Pong" [parseRoutes|
/register RegisterR GET
/registered AccRegisterR POST
/login   LoginR GET POST
/lobby/#String   LobbyR GET
|]

data User = User
    { username      :: Text
    , password      :: Text
    }
  deriving Show

data NewPerson = NewPerson
    { newUsername         :: Text
    , newPassword         :: Text
    , confirmPassword     :: Text
    }
  deriving Show


createNew un pw = do
  id <- runDB $ (insert $ Person un pw 0 0 0)
  return id
  
getUser un pw = do
  u <- runDB $ selectFirst [PersonUsername ==. un, PersonPassword ==. pw] []
  case u of
    Just person -> return $ Right person
    Nothing -> return $ Left ("Login failed.." :: Text)
   
--userLoss un = do
 --updateWhere [PersonUsername ==. un] [PersonLoss +=. 1]
 --updateWhere [PersonUsername ==. un] [PersonGamespl +=. 1]

--userWin un = do
 --updateWhere [PersonUsername ==. un] [PersonWin +=. 1]
 --updateWhere [PersonUsername ==. un] [PersonGamespl +=. 1]


userForm :: Html -> MForm Handler (FormResult User, Widget)
userForm = renderDivs $ User
    <$> areq textField     "Username: " Nothing
    <*> areq passwordField "Password: " Nothing

newAccountForm :: Html -> MForm Handler (FormResult NewPerson, Widget)
newAccountForm = renderDivs $ NewPerson
    <$> areq textField     "Username: " Nothing
    <*> areq passwordField "Password: " Nothing
    <*> areq passwordField "Confirm: " Nothing

_GAME_TICK_DELAY = 100000

--updateGame :: Game -> IO (Game)
--updateGame game = (tick game) >>= \g -> do
--    print "im called update"
--    if needsLogging g
--        then do
--            print "needs logging"
--            let maybeWinLosePair = getWinLose g
--            -- maybeWinLosePair is Maybe (a, b) where (a, b) is a tuple
--            -- update database maybe in a different thread that kills itself
--            -- maybe pass along the gameChan so a db updated message can also be seen by users
--            -- in order for users to be updated, another message type would be needed
--            setCompleted g
--        else do 
--            print "doesnt need logging" 
--            return g

--updateGames :: TVar [IO Game] -> IO ()
--updateGames games = forever $ do
--    gs <- readTVarIO games
--    mapM_ (\iog -> iog >>= (\g -> do
--        ch <- readIORef (gameChan g)
--        atomically $ writeTChan ch (newChatMsg "in game now bitches"))) gs
--    atomically $ modifyTVar games (\gs -> map (\iog -> iog >>= updateGame) gs)
--    threadDelay _GAME_TICK_DELAY

updateGame :: IO (Game, TChan Message) -> IO (Game, TChan Message)
updateGame gameAndGameChan = do
    pair <- gameAndGameChan
    let game = tick (fst pair)
    let chan = snd pair
    printGame game
    atomically $ writeTChan chan (getGameMsg game)
    if (needsLogging game)
        then do
            print "needs logging"
            let maybeWinLosePair = getWinLose game
            -- maybeWinLosePair is Maybe (a, b) where (a, b) is a tuple
            -- update database maybe in a different thread that kills itself
            -- maybe pass along the gameChan so a db updated message can also be seen by users
            -- in order for users to be updated, another message type would be needed
            let game' = setCompleted game
            return (game', chan)
        else do
            print "doesn't need logging"
            return (game, chan)

updateGames :: TVar [IO (Game, TChan Message)] -> IO ()
updateGames games = forever $ do
    gs <- readTVarIO games
    mapM_ (\iog -> iog >>= (\p -> do
        let ch = snd p
        atomically $ writeTChan ch (newMessage GarbageMsg (toJSON Garbage)))) gs
    print $ length gs
    atomically $ modifyTVar games (\gs -> map (\pair -> updateGame pair) gs)
    threadDelay _GAME_TICK_DELAY

-- call this function to make a given user join a game
-- it duplicates the games channel into the user's game channel IORef
-- gameChannel is the user's gameChannel IORef, which is created in appHandler
joinGame :: String -> Pong -> IO ()
joinGame username app = do
    gamesLength <- readTVarIO (nextGameId app)
    userGameIdMap <- readTVarIO (userToGameId app)
    userGameChannels <- readTVarIO (userGameChannels app)
    let maybeGameChan = Map.lookup username userGameChannels
    let gameId = Map.lookup username userGameIdMap
    if (isJust gameId && isJust maybeGameChan)
        then do
            let gameAndGameChan = getGame (fromJust gameId) gamesLength (games app)
            if (isJust gameAndGameChan)
                then do
                    pair <- fromJust gameAndGameChan
                    let ch = snd pair
                    let gameCh = fromJust maybeGameChan
                    --ch <- (fromJust game) >>= (\g -> readIORef (gameChan g))
                    duppedCh <- atomically $ dupTChan ch
                    modifyIORef gameCh (\_ -> Just duppedCh)
                    putStr $ "channel successfully duped for "
                    atomically $ writeTChan ch $ newMessage ReadyMsg (toJSON Ready)
                else return ()
        else return ()

-- leaves the game by clearing the game channel IORef
leaveGame :: IORef (Maybe (TChan Message)) -> IO ()
leaveGame gameChannel = do
    modifyIORef gameChannel (\_ -> Nothing)

getGame :: Int -> Int -> TVar [IO (Game, TChan Message)] -> Maybe (IO (Game, TChan Message))
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
    --liftIO $ print "========ISINGAME======="
    --liftIO $ print isInGame
    --liftIO $ putStr " FOR "
    --liftIO $ print username
    --liftIO $ print "========ISINGAME======="
    msg <- liftIO $ if isInGame
        then atomically $ (readTChan $ fromJust maybeGameChan) `orElse` (readTChan chatChan)
        else atomically $ readTChan chatChan
    --liftIO $ print "========THE_MSG======="
    --liftIO $ print msg
    --liftIO $ putStr " FOR "
    --liftIO $ print username
    --liftIO $ print "========THE_MSG======="
    if ((msgType msg) == ChallengeMsg)
        then do
            let challenge = decode $ encode $ msgData msg :: Maybe Challenge
            if ((isJust challenge) && ((challenged (fromJust challenge)) == (pack username)))
                then yieldMsg msg
                else yield empty
        else yieldMsg msg
  where yieldMsg msg = yield $ toStrict $ encode $ msg
    
handleChat app fromPlayer msg = do 
    liftIO $ atomically $ writeTChan (chatChan app) $ newMessage ChatMsg (toJSON $ (Chat fromPlayer msg))

moveGamePaddle' :: [IO (Game, TChan Message)] -> Int -> String -> Direction -> [IO (Game, TChan Message)]
moveGamePaddle' games gid username direction = do
    let replaceOp = (\gameAndGameChan -> gameAndGameChan >>= (\pair -> return ((movePaddle (fst pair) username direction), snd pair)))
    replace' gid replaceOp games

moveGamePaddle :: TVar [IO (Game, TChan Message)] -> Int -> String -> Direction -> IO ()
moveGamePaddle games gid username direction = do
    atomically $ modifyTVar games (\gs -> moveGamePaddle' gs gid username direction)

handleMove app player direction = do
    let pid = unpack player 
    myMap <- liftIO $ readTVarIO (userToGameId app)
    let gameId = Map.lookup pid myMap
    if isJust gameId
        then do
            let gid = fromJust gameId
            case direction of
                "up" -> liftIO $ moveGamePaddle (games app) gid pid Up
                "down" -> liftIO $ moveGamePaddle (games app) gid pid Down
            return ()
        else
            return ()
handleChallenge app player1 player2 = do
    liftIO $ atomically $ modifyTVar (challengedToChallenger app) (\s -> Map.insert (unpack player2) (unpack player1) s)
    liftIO $ atomically $ writeTChan (chatChan app) $ newMessage ChallengeMsg (toJSON $ (Challenge player1 player2))

createGame :: Pong -> String -> String -> IO ()
createGame app lPlayer rPlayer = do
    atomically $ do
        currentGameId <- readTVar (nextGameId app)
        modifyTVar (nextGameId app) (\n -> n + 1)
        chan <- newBroadcastTChan
        modifyTVar (games app) (\gs -> gs ++ [return (initGame lPlayer rPlayer, chan)])
        modifyTVar (userToGameId app) (\mapping -> Map.insert lPlayer currentGameId mapping)
        modifyTVar (userToGameId app) (\mapping -> Map.insert rPlayer currentGameId mapping)
    joinGame lPlayer app
    joinGame rPlayer app

handleAccept app acceptingPlayer = do
    let acceptingPlayerU = unpack acceptingPlayer
    myMap <- liftIO $ readTVarIO (challengedToChallenger app)
    let challenger = Map.lookup acceptingPlayerU myMap
    --currentGameId <- liftIO $ readTVarIO (nextGameId app)
    if isJust challenger
        then do
            let challengingPlayer = fromJust challenger
            liftIO $ atomically $ modifyTVar (challengedToChallenger app) (\s -> Map.delete acceptingPlayerU s) 
            liftIO $ createGame app challengingPlayer acceptingPlayerU
            
            usersOnline2 <- liftIO $ readTVarIO (usersOnline app)
            liftIO $ atomically $ writeTChan (chatChan app) $  newMessage UsersOnlineMsg (toJSON $ (UsersOnline usersOnline2 ))
    
            userToGameId2 <- liftIO $ readTVarIO (userToGameId app)
            liftIO $ atomically $ writeTChan (chatChan app) $  newMessage UsersInGameMsg (toJSON $ (UsersInGame $ Map.keys userToGameId2 ))

            --liftIO $ joinGame challengingPlayer app 
            --liftIO $ joinGame acceptingPlayerU app

            --gamesLength <- liftIO $ readTVarIO (nextGameId app)
            --gameAndGameChan <- liftIO $ fromJust $ getGame (fromJust gameId) gamesLength (games app)
            --let game = fst gameAndGameChan
            --let ch = snd gameAndGameChan
            return ()
        else
            liftIO $ atomically $ writeTChan (chatChan app) $ newMessage ChallengeExpMsg (toJSON $ (ChallengeExp acceptingPlayer))
    
handleLeave app msg = do
    --need to actually implement this
    liftIO $ atomically $ writeTChan (chatChan app) $ newChatMsg "leave"

handleReady app player = do
    let pid = unpack player 
    myMap <- liftIO $ readTVarIO (userToGameId app)
    let gameId = Map.lookup pid myMap
    if isJust gameId
        then do
            --gamesLength <- liftIO $ readTVarIO (nextGameId app)
            --gameAndGameChan <- liftIO $ fromJust $ getGame (fromJust gameId) gamesLength (games app)
            --let game = fst gameAndGameChan
            --liftIO $ print (gameStatus game)
            let gid = fromJust gameId
            let replaceOp = (\gameAndGameChan -> gameAndGameChan >>= (\pair -> return (setReady (fst pair) (unpack player), snd pair)))
            liftIO $ atomically $ modifyTVar (games app) (\gs ->
                replace' gid replaceOp gs)
            --game <- liftIO $ fromJust $ getGame (fromJust gameId) gamesLength (games app)
            --liftIO $ print (gameStatus game)
        else
            return ()

--setGameReady :: [IO (Game, TChan Message)] -> Int -> Text -> [IO (Game, TChan Message)]
--setGameReady games gid player = do 
--    let gameAndGameChan' = (games !! gid) >>= (\pair -> ((setReady (fst pair) (unpack player)), snd pair))
--    replace' gid (return gameAndGameChan') games

replace' :: Int -> (a -> a) -> [a] -> [a]
replace' index op list = (take index list) ++ [op (list !! index)] ++ (drop (index+1) list)

handleOther app msg = do
    return ()

handleMsg app msg = do
    let msgParsed = splitOn "`" msg
    let msgType = msgParsed !! 0
    case msgType of 
        "chat" -> 
            handleChat app (msgParsed !! 1) (msgParsed !! 2)  
        "move" -> 
            handleMove app (msgParsed !! 1) (msgParsed !! 2) 
        "challenge" -> 
            handleChallenge app (msgParsed !! 1) (msgParsed !! 2) 
        "accept" -> 
            handleAccept app (msgParsed !! 1)
        "leave" -> 
            handleLeave app msg
        "ready" ->
            handleReady app (msgParsed !! 1)
        _ -> 
            handleOther app msg
    mySet <- liftIO $ readTVarIO (usersOnline app)
    liftIO $ print $ Set.toList mySet
    myMap <- liftIO $ readTVarIO (userToGameId app)
    liftIO $ print $ Map.toList myMap
    myMap2 <- liftIO $ readTVarIO (challengedToChallenger app)
    liftIO $ print $ Map.toList myMap2
    
appHandler :: String -> WebSocketsT Handler ()
appHandler username = do
    app <- getYesod
    gameChannel <- liftIO $ newIORef Nothing
    
    liftIO $ atomically $ modifyTVar (usersOnline app) (\s -> Set.insert username s)

    liftIO $ atomically $ modifyTVar (userGameChannels app) (\s -> Map.insert username gameChannel s)
    chatChannel <- liftIO $ atomically $ dupTChan (chatChan app)
    let msgs = msgSource username gameChannel chatChannel
    let msgHandler = handleMsg app
    usersOnline2 <- liftIO $ readTVarIO (usersOnline app)
    liftIO $ atomically $ writeTChan (chatChan app) $  newMessage UsersOnlineMsg (toJSON $ (UsersOnline usersOnline2 ))
    userToGameId2 <- liftIO $ readTVarIO (userToGameId app)
    liftIO $ atomically $ writeTChan (chatChan app) $  newMessage UsersInGameMsg (toJSON $ (UsersInGame $ Map.keys userToGameId2 ))
    race_
        (msgs $$ sinkWSText)
        (sourceWS $$ mapM_C msgHandler)


getLoginR :: Handler Html
getLoginR = do
    (widget, enctype) <- generateFormPost userForm
    --people <- runDB $ selectList [] [Asc PersonWin]
    defaultLayout
        [whamlet|
            <h1>Login to Pong Web App</h1>
            <a href=@{RegisterR}>Click to register for an acount!
            <p>
                Login with your username and password.
                <form method=post action=@{LoginR} enctype=#{enctype}>
                    ^{widget}
                    <button>Login
        |]


getLobbyR :: String -> Handler Html
getLobbyR username = do
    webSockets (appHandler username)
    defaultLayout $ do
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.7.0/jquery.js"
        [whamlet|
            <div .content #content>
                <div .game #game>
                    <div .paddleA #paddleA></div>
                    <div .paddleB #paddleB></div>
                    <div .ball #ball>
            <div .chatBox #chatBox>
            <div .usersOnLine #usersOnLine>
            <input .message #message type=text>
            <button .send #send type="button" onclick="sendChatMsg()">Send

        |]
        toWidget [lucius|
            .chatBox {
                position: absolute;
                width: 1853px;
                height: 225px;
                border: 1px teal solid;
                float:left;
                margin-top:1px;
                bottom: 80px;
                left: 0;
                overflow: scroll;
            }
            .usersOnLine{
                position: absolute;
                right: 0;
                top: 0;
                font-family:tahoma;
                font-size:14px;
                color:orange;
                border:1px teal solid;
                width:300px;
                height:677px;
                overflow:scroll;
                margin-left:1px;
                
            }
            .message {
                position: absolute;
                width: 1756px;
                height: 80px;
                border: 1px teal solid;
                float:left;
                margin-top:1px;
                bottom: 0;
                left: 0;
            }
            .send {
                position: absolute;
                width: 100px;
                height: 80px;
                float:left;
                margin:1px;
                bottom: 0;
                right: 0;
            }
            .game {
                background: #daeff5;
                width: 1550px;
                height: 676px;
                position: absolute;
                left: 0;
                top: 0;
                margin: 0 auto;
                overflow: hidden;
                border-radius: 5px;
                border: 2px solid #ffffff
            }
            .ball {
                background: #d9ed7a;
                position: absolute;
                width: 20px;
                height: 20px;
                left: 777px;
                top: 340px;
                border-radius: 10px;
                border: 1px solid #000000
            }
            .paddleA {
                background: #d5d3d3;
                position: absolute;
                width: 15px;
                height: 70px;
                left: 0px;
                top: 340px;
                border-radius: 5px;
                border: 1px solid #000000
            }
            .paddleB {
                background: #d5d3d3;
                position: absolute;
                width: 15px;
                height: 70px;
                left: 1539px;
                top: 340px;
                border-radius: 5px;
                border: 1px solid #000000
            }
        |]
        toWidget [julius|
            var url = document.URL, conn;

            url = url.replace("http:", "ws:").replace("https:", "wss:");
            conn = new WebSocket(url);

            var usersOnlineLocal = [];
            var username;
            var pos = 0;
            var playerInGame = false;

            window.onload = function(e){
                var URLparts = (document.URL).split("/");
                username = URLparts[URLparts.length-1];
            };

            conn.onmessage = function(e) {
                message = JSON.parse(e.data);
                console.log("new message has arrived");
                switch(message.msgType){
                    case "ChallengeMsg":
                        challengePopUp(message.msgData.challenger);
                        break;
                    case "ChatMsg":
                        addChatMsg(message.msgData.fromUser,message.msgData.chatData);
                        break;
                    case "ReadyMsg":
                        readyPopUp();
                        break;
                    case "GameMsg":
                        processGameState(message.msgData);
                        break;
                    case "UsersOnlineMsg":
                        updateUsersOnlineList(message.msgData.usersOL);
                        break;
                    case "UsersInGameMsg":
                        updateUsersInGameList(message.msgData.usersIG);
                        break;
                    default:
                        console.log("unknown message");
                        break;
                }
            };

            function sendMessageToServer(message){
                conn.send(message);
            };

            function updateUsersOnlineList(usersOnlineList){
                usersOnlineLocal = usersOnlineList;
                usersOnLine.innerHTML=""
                for(i=0;i<usersOnlineList.length;i++){
                    user = document.createElement("span");
                    user.innerHTML=usersOnlineList[i];
                    usersOnLine.appendChild(user);
                    brLine = document.createElement("br");
                    usersOnLine.appendChild(brLine)
                }
            };

            function updateUsersInGameList(usersInGame){
                var childSpans = usersOnLine.getElementsByTagName('span');
                for(i=0; i< childSpans.length; i++ ){
                    if(usersInGame.indexOf(childSpans[i].innerHTML) > (-1)){
                        childSpans[i].innerHTML = childSpans[i].innerHTML+" (Ingame)";
                    }
                    else if (!(childSpans[i].innerHTML.indexOf("Ingame")> (-1))) {
                        childSpans[i].innerHTML = "<a onclick=createChallengePopup("+JSON.stringify(childSpans[i].innerHTML)+") href>"+childSpans[i].innerHTML+"</a>"
                    }
                }
            };  

            function addChatMsg(fromUser,message){
                msg = document.createElement("span");
                msg.innerHTML=fromUser+": " + message;
                chatBox.appendChild(msg);
                brLine = document.createElement("br");
                chatBox.appendChild(brLine)
            };  

            function challengePopUp(challenger){
                var accept = confirm("Accept the challenge from "+challenger+"?");
                if (accept == true) {
                    acceptChallenge();
                    console.log("You accepted!");
                } else {
                    console.log("You denied!");
                } 
            };

            function readyPopUp(){
                var ready = confirm("Ready to start the game?");
                if (ready == true) {
                    declareReady();
                    console.log("You are ready!");
                } else {
                    console.log("You are not ready!");
                } 
            };

            function processGameState(gameData){
                console.log("ProcessGameState is called");
                if(gameData.gameStatus == "Playing"){
                    playerInGame = true;
                    console.log("Moving paddleL to "
                        +gameData.state.lPaddle.py
                        +" Moving paddleR to "
                        +gameData.state.rPaddle.py
                        +" Moving ball to ("
                        +gameData.state.ball.bx+","
                        +gameData.state.ball.by+")");
                    updatePaddlesPos(gameData.state.rPaddle.py,gameData.state.lPaddle.py)
                    updateBallPos(gameData.state.ball.bx,gameData.state.ball.by)
                }
            };

            function sendChallenge(challengee){
                sendMessageToServer("challenge`"+username+"`"+challengee);
            };

            function sendChatMsg(){
                msg = document.getElementById("message").value;
                sendMessageToServer("chat`"+username+"`"+msg);
                document.getElementById("message").value = "";
            };

            function acceptChallenge(){
                sendMessageToServer("accept`"+username);
            };

            function declareReady(){
                sendMessageToServer("ready`"+username);
            };

            function movePaddle(direction){
                sendMessageToServer("move`"+username+"`"+direction);
            }

            function leaveGame(){
            };

            function logout(){
            };


            function createChallengePopup(challengee){
                var createChallenge = confirm("Want to challenge "+challengee+"?");
                if (createChallenge == true) {
                    sendChallenge(challengee);
                }
            }

            document.onkeydown=function(e)
            {
                pos=1;
                key=window.event?e.keyCode:e.which;
            };

            document.onkeyup=function(e){pos=0;};

            setInterval(function()
            {
                if ($(document.getElementById("message")).is( ":focus" ) || (pos == 0) || !playerInGame)
                    return;
                if(key==38){
                    movePaddle("up");
                }
                if(key==40)
                    movePaddle("down");
            },100);

            var ball = { speed: 3, x: 550, y: 230, directionX: 1, directionY: 1 };

            function updatePaddlesPos(lPos,rPos) {
                $("#paddleA").css("top", parseInt(lPos));
                $("#paddleB").css("top", parseInt(rPos));
            };

            function updateBallPos(xPos, yPos) {
                $("#ball").css({ "left": parseInt(xPos), "top": parseInt(yPos) });
            };
        |]

--getPersonR :: PersonId -> Handler String
--getPersonR personId = do
 --   person <- runDB $ get404 personId
 --   return $ show person

getRegisterR :: Handler Html
getRegisterR = do
    -- Generate the form to be displayed
    (widget, enctype) <- generateFormPost newAccountForm
    defaultLayout
        [whamlet|
            <h1>Register to Pong Web App</h1>
            <form method=post action=@{AccRegisterR} enctype=#{enctype}>
                ^{widget}
                <button>Register
        |]

postAccRegisterR :: Handler Html
postAccRegisterR = do
    ((result, widget), enctype) <- runFormPost newAccountForm
    case result of
        FormSuccess newPerson -> do 
            let a = unpack(newUsername newPerson)
            let b = unpack(newPassword newPerson)
            -- CREATE NEW USER *****
            id <- createNew a b
            --id <- runDB $ (insert $ Person a b 0 0 0)
            defaultLayout [whamlet|<p>Register Result:<p>#{show newPerson}|]
            redirect LoginR
        _ -> defaultLayout
            [whamlet|
                <h1>Uh oh, something went wrong with the Registration POST request.</h1>
                <p>Invalid input, let's try again.
                <form method=post action=@{AccRegisterR} enctype=#{enctype}>
                    ^{widget}
                    <button>RegisterAttempt2
            |]


postLoginR :: Handler Html
postLoginR = do
    ((result, widget), enctype) <- runFormPost userForm
    case result of
--      FormSuccess user -> defaultLayout [whamlet|<p>Login Result:<p>#{show user}|]
        FormSuccess user -> do
            let postedUsername = unpack(username user)
            let postedPassword = unpack(password user)
            authResult <- getUser postedUsername postedPassword
            -- AUTHENTICATE***
            redirect (LobbyR $ postedUsername)
        --defaultLayout [whamlet|<p>Login Result:<p>#{show user}|]
        _ -> defaultLayout
            [whamlet|
                <h1>Uh oh, something went wrong with the Login POST request.</h1>
                <p>Invalid input, let's try again.
                <form method=post action=@{LoginR} enctype=#{enctype}>
                    ^{widget}
                    <button>LoginAttempt2
            |]
            
openConnectionCount :: Int
openConnectionCount = 10            
main :: IO ()
main = do
    let conf = SqliteConf "test.db3" openConnectionCount
    pool <- createPoolConfig conf
    flip runSqlPersistMPool pool $ do
        runMigration migrateAll
--main = runStderrLoggingT $ withSqlitePool "test.db3" openConnectionCount
--  $ \pool -> liftIO $ do
--    runResourceT $ flip runSqlPool pool $ do
--         runMigration migrateAll
    games <- newTVarIO ([] :: [IO (Game, TChan Message)])
    nextGameId <- newTVarIO (0 :: Int)
    chatChan <- atomically newBroadcastTChan
    userToGameId <- newTVarIO (Map.empty)
    challengedToChallenger <- newTVarIO (Map.empty)
    usersOnline <- newTVarIO (Set.empty)
    userGameChannels <- newTVarIO (Map.empty)
    print $ encode $ toJSON $ newChatMsg "hello world"
    forkIO $ updateGames games
    warp 3000 $ Pong conf pool games nextGameId chatChan userToGameId challengedToChallenger usersOnline userGameChannels
