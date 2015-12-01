{-# LANGUAGE EmptyDataDecls, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ViewPatterns, TypeSynonymInstances, QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings, DeriveGeneric #-}
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
import Database.Persist
--import Database.Persist.Sqlite
import Database.Persist.Sql
import Database.Persist.TH
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Reader
import Control.Lens
import qualified Data.Conduit.List as CL

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
    usersOnline :: TVar (Set.Set String),
    --map for user to IORef to game channel
    userGameChannels :: TVar (Map.Map String (IORef (Maybe (TChan Message)))),

    counter :: TVar Int
}

-- Define our entities as usual
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
 fullname String
 username String
 UsernameKey username
 password String
 gamespl Int
 win Int
 loss Int
    deriving Show
|]

-- We keep our connection pool in the foundation. At program initialization, we
-- create our initial pool, and each time we need to perform an action we check
-- out a single connection from the pool.
--data Pong = Pong ConnectionPool

-- We'll create a single route, to access a person. It's a very common
-- occurrence to use an Id type in routes.
mkYesod "Pong" [parseRoutes|
/register RegisterR GET
/registered AccRegisterR POST
/login   LoginR GET POST
/lobby   LobbyR GET
|]

-- Nothing special here
instance Yesod Pong
-- Now we need to define a YesodPersist instance, which will keep track of
-- which backend we're using and how to run an action.

instance YesodJquery Pong

instance RenderMessage Pong FormMessage where
    renderMessage _ _ = defaultFormMessage

--instance YesodPersist Pong where
--    type YesodPersistBackend Pong = SqlBackend
--    runDB action = do
--        Pong pool <- getYesod
--        runSqlPool action pool

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

userForm :: Html -> MForm Handler (FormResult User, Widget)
userForm = renderDivs $ User
    <$> areq textField     "Username: " Nothing
    <*> areq passwordField "Password: " Nothing

newAccountForm :: Html -> MForm Handler (FormResult NewPerson, Widget)
newAccountForm = renderDivs $ NewPerson
    <$> areq textField     "Username: " Nothing
    <*> areq passwordField "Password: " Nothing
    <*> areq passwordField "Confirm: " Nothing

_GAME_TICK_DELAY = 1000000

updateGame :: Game -> IO Game
updateGame game = (tick game) >>= \g -> do
    print "im called update"
    if needsLogging g
        then do
            print "needs logging"
            let maybeWinLosePair = getWinLose g
            -- maybeWinLosePair is Maybe (a, b) where (a, b) is a tuple
            -- update database maybe in a different thread that kills itself
            -- maybe pass along the gameChan so a db updated message can also be seen by users
            -- in order for users to be updated, another message type would be needed
            setCompleted g
        else do 
            print "doesnt need logging" 
            return g

updateGames :: TVar [IO Game] -> IO ()
updateGames games = forever $ do
    gs <- readTVarIO games
    mapM_ (\iog -> iog >>= (\g -> atomically $ writeTChan (gameChan g) (newChatMsg "in game now bitches"))) gs
--    atomically $ modifyTVar games (\gs -> map (\iog -> iog >>= updateGame) gs)
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
                    putStr $ "channel successfully dupped for "
                    atomically $ writeTChan ch $ newChatMsg $ "Pop Up Ready"
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
    liftIO $ print "========ISINGAME======="
    liftIO $ print isInGame
    liftIO $ putStr " FOR "
    liftIO $ print username
    liftIO $ print "========ISINGAME======="
    msg <- liftIO $ if isInGame
        then atomically $ (readTChan $ fromJust maybeGameChan) `orElse` (readTChan chatChan)
        else atomically $ readTChan chatChan
    liftIO $ print "========THE_MSG======="
    liftIO $ print msg
    liftIO $ putStr " FOR "
    liftIO $ print username
    liftIO $ print "========THE_MSG======="
    if ((msgType msg) == ChallengeMsg)
        then do
            let challenge = decode $ encode $ msgData msg :: Maybe Challenge
            if ((isJust challenge) && ((challenged (fromJust challenge)) == (pack username)))
                then yieldMsg msg
                else yield empty
        else yieldMsg msg
  where yieldMsg msg = yield $ toStrict $ encode $ msg
    
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
                "up" -> liftIO $ movePaddle game pid Up
                "down" -> liftIO $ movePaddle game pid Down
            liftIO $ atomically $ writeTChan (chatChan app) $ newChatMsg "move"
        else
            liftIO $ atomically $ writeTChan (chatChan app) $ newChatMsg "move"
    
handleChallenge app player1 player2 = do
    liftIO $ atomically $ modifyTVar (challengedToChallenger app) (\s -> Map.insert (unpack player2) (unpack player1) s)
    liftIO $ atomically $ writeTChan (chatChan app) $ newMessage ChallengeMsg (toJSON $ (Challenge player1 player2))
    
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
            
            userGameChannelsMap <- liftIO $ readTVarIO (userGameChannels app)
            let player1GC = Map.lookup (fromJust challenger) userGameChannelsMap
            let player2GC = Map.lookup acceptingPlayerU userGameChannelsMap
            
            liftIO $ joinGame (fromJust challenger) (fromJust player1GC) app 
            liftIO $ joinGame acceptingPlayerU (fromJust player2GC) app

            gamesLength <- liftIO $ readTVarIO (nextGameId app)
            let game = liftIO $ fromJust $ getGame currentGameId gamesLength (games app)
            ch <-  game >>= (\g -> return (gameChan g))
            liftIO $ print "game should be okay"
            liftIO $ atomically $ writeTChan (chatChan app) $ newChatMsg "you can proceed to the game!"
        else
            liftIO $ atomically $ writeTChan (chatChan app) $ newChatMsg "accepted a nonexisting challenge"
    
handleLeave app msg = do
    --need to actually implement this
    liftIO $ atomically $ writeTChan (chatChan app) $ newChatMsg "leave"

handleReady app player = do
    let pid = unpack player 
    myMap <- liftIO $ readTVarIO (userToGameId app)
    let gameId = Map.lookup pid myMap
    if isJust gameId
        then do
            gamesLength <- liftIO $ readTVarIO (nextGameId app)
            game <- liftIO $ fromJust $ getGame (fromJust gameId) gamesLength (games app)
            liftIO $ print (gameStatus game)
            let newGame = liftIO $ setReady game pid
            liftIO $ atomically $ modifyTVar (games app) (\gs -> set (element  (fromJust gameId)) (newGame) gs)
            game <- liftIO $ fromJust $ getGame (fromJust gameId) gamesLength (games app)
            liftIO $ print (gameStatus game)
        else
            return ()

handleOther app msg = do
    liftIO $ atomically $ writeTChan (chatChan app) $ newChatMsg ""

handleMsg app msg = do
    let msgParsed = splitOn "`" msg
    let msgType = msgParsed !! 0
    case msgType of 
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
    
appHandler :: WebSocketsT Handler ()
appHandler = do
    sess <- getSession
    app <- getYesod
    gameChannel <- liftIO $ newIORef Nothing
    
    --instead of using counter, should just get last session popped onto the sessionMap
    liftIO $ print $ Map.toList sess
    counterNum <- liftIO $ readTVarIO (counter app)
    username <- lookupSession $ pack $ show counterNum
    liftIO $ atomically $ modifyTVar (usersOnline app) (\s -> Set.insert (unpack (fromJust username)) s)

    liftIO $ atomically $ modifyTVar (counter app) (\n -> n + 1)

    liftIO $ atomically $ modifyTVar (userGameChannels app) (\s -> Map.insert (unpack (fromJust username)) gameChannel s)
    chatChannel <- liftIO $ atomically $ dupTChan (chatChan app)
    let msgs = msgSource ( unpack (fromJust username)) gameChannel chatChannel
    let msgHandler = handleMsg app
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


postLoginR :: Handler Html
postLoginR = do
    ((result, widget), enctype) <- runFormPost userForm
    case result of
--      FormSuccess user -> defaultLayout [whamlet|<p>Login Result:<p>#{show user}|]
        FormSuccess user -> do
            let postedUsername = (username user)
            let postedPassword = (password user)
            --authResult <- getUser postedUsername postedPassword
            setSession "0" "sheryan"
            setSession "1" "harrison"
            setSession "2" "eugene"
            setSession "3" "miraj"
            --(key, mval) <- runInputPost $ (,) <$> ireq textField "key" <*> iopt textField "val"
            --setSession postedUsername postedPassword
            redirect LobbyR
        --Sheryan
        --case mval of
        --    Nothing -> deleteSession key
        --    Just val -> setSession key val
        --liftIO $ print (key, mval)
        --msg <- runInputPost $ ireq textField "key"
        -- Miraj
        --defaultLayout [whamlet|<p>Login Result:<p>#{show user}|]
        _ -> defaultLayout
            [whamlet|
                <h1>Uh oh, something went wrong with the Login POST request.</h1>
                <p>Invalid input, let's try again.
                <form method=post action=@{LoginR} enctype=#{enctype}>
                    ^{widget}
                    <button>LoginAttempt2
            |]

getLobbyR :: Handler Html
getLobbyR = do
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
        FormSuccess newPerson -> defaultLayout [whamlet|<p>Register Result:<p>#{show newPerson}|]
--createNew "name" (username newPerson) (password newPerson)
        _ -> defaultLayout
            [whamlet|
                <h1>Uh oh, something went wrong with the Registration POST request.</h1>
                <p>Invalid input, let's try again.
                <form method=post action=@{AccRegisterR} enctype=#{enctype}>
                    ^{widget}
                    <button>RegisterAttempt2
            |]

main :: IO ()
main = do
    games <- newTVarIO ([] :: [IO Game])
    nextGameId <- newTVarIO (0 :: Int)
    counter <- newTVarIO (0 :: Int)
    chatChan <- atomically newBroadcastTChan
    userToGameId <- newTVarIO (Map.empty)
    challengedToChallenger <- newTVarIO (Map.empty)
    usersOnline <- newTVarIO (Set.empty)
    userGameChannels <- newTVarIO (Map.empty)
    print $ encode $ toJSON $ newChatMsg "hello world"
    forkIO $ updateGames games
    warp 3000 $ Pong games nextGameId chatChan userToGameId challengedToChallenger usersOnline userGameChannels counter

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
