{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
import Yesod
import Yesod.Form.Jquery
import Data.Text (Text)
import Database.Persist.Sqlite
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)

-- Define our entities as usual
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    firstName String
    lastName String
    age Int
    deriving Show
|]

-- We keep our connection pool in the foundation. At program initialization, we
-- create our initial pool, and each time we need to perform an action we check
-- out a single connection from the pool.
data PersistTest = PersistTest ConnectionPool

-- We'll create a single route, to access a person. It's a very common
-- occurrence to use an Id type in routes.
mkYesod "PersistTest" [parseRoutes|
/ HomeR GET
/register RegisterR GET
/registered AccRegisterR POST
/person/#PersonId PersonR GET
/login   LoginR POST
|]

-- Nothing special here
instance Yesod PersistTest
-- Now we need to define a YesodPersist instance, which will keep track of
-- which backend we're using and how to run an action.

instance YesodJquery PersistTest

instance RenderMessage PersistTest FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodPersist PersistTest where
    type YesodPersistBackend PersistTest = SqlBackend
    runDB action = do
        PersistTest pool <- getYesod
        runSqlPool action pool

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

-- List all people in the database
getHomeR :: Handler Html
getHomeR = do
    (widget, enctype) <- generateFormPost userForm
    people <- runDB $ selectList [] [Asc PersonAge]
    defaultLayout
        [whamlet|
            <h1>Login to Pong Web App</h1>
            <a href=@{RegisterR}>Click to register for an acount!
            <p>
                Login with your username and password.
                <form method=post action=@{LoginR} enctype=#{enctype}>
                    ^{widget}
                    <button>Login
            <ul>
                $forall Entity personid person <- people
                    <li>
                        <a href=@{PersonR personid}>#{personFirstName person}
        |]

-- We'll just return the show value of a person, or a 404 if the Person doesn't
-- exist.
getPersonR :: PersonId -> Handler String
getPersonR personId = do
    person <- runDB $ get404 personId
    return $ show person

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

-- The POST handler processes the form. If it is successful, it displays the
-- parsed person. Otherwise, it displays the form again with error messages.
postLoginR :: Handler Html
postLoginR = do
    ((result, widget), enctype) <- runFormPost userForm
    case result of
        FormSuccess user -> defaultLayout [whamlet|<p>Login Result:<p>#{show user}|]
        _ -> defaultLayout
            [whamlet|
                <h1>Uh oh, something went wrong with the Login POST request.</h1>
                <p>Invalid input, let's try again.
                <form method=post action=@{LoginR} enctype=#{enctype}>
                    ^{widget}
                    <button>LoginAttempt2
            |]

postAccRegisterR :: Handler Html
postAccRegisterR = do
    ((result, widget), enctype) <- runFormPost newAccountForm
    case result of
        FormSuccess newPerson -> defaultLayout [whamlet|<p>Register Result:<p>#{show newPerson}|]
        _ -> defaultLayout
            [whamlet|
                <h1>Uh oh, something went wrong with the Registration POST request.</h1>
                <p>Invalid input, let's try again.
                <form method=post action=@{AccRegisterR} enctype=#{enctype}>
                    ^{widget}
                    <button>RegisterAttempt2
            |]

openConnectionCount :: Int
openConnectionCount = 10
main :: IO ()
main = runStderrLoggingT $ withSqlitePool "test.db3" openConnectionCount
  $ \pool -> liftIO $ do
    runResourceT $ flip runSqlPool pool $ do
        runMigration migrateAll
        insert $ Person "Michael" "Snoyman" 26
    warp 3000 $ PersistTest pool