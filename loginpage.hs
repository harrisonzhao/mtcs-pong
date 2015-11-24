{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Control.Applicative ((<$>), (<*>))
import           Data.Text           (Text)
import           Data.Time           (Day)
import           Yesod
import           Yesod.Form.Jquery

data App = App

mkYesod "App" [parseRoutes|
/         HomeR GET
/register RegisterR GET
/person   PersonR POST
|]

instance Yesod App

-- Tells our application to use the standard English messages.
-- If you want i18n, then you can supply a translating function instead.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- And tell us where to find the jQuery libraries. We'll just use the defaults,
-- which point to the Google CDN.
instance YesodJquery App

-- The datatype we wish to receive from the login form
data Person = Person
    { personUsername      :: Text
    , personPassword      :: Text
    }
  deriving Show

-- The datatype we wish to receive from the registration form
data NewPerson = NewPerson
    { newUsername         :: Text
    , newPassword         :: Text
    , confirmPassword     :: Text
    }
  deriving Show

-- Declare the form. The type signature is a bit intimidating, but here's the
-- overview:
--
-- * The Html parameter is used for encoding some extra information. See the
-- discussion regarding runFormGet and runFormPost below for further
-- explanation.
--
-- * We have our Handler as the inner monad, which indicates which site this is
-- running in.
--
-- * FormResult can be in three states: FormMissing (no data available),
-- FormFailure (invalid data) and FormSuccess
--
-- * The Widget is the viewable form to place into the web page.
--
-- Note that the scaffolded site provides a convenient Form type synonym,
-- so that our signature could be written as:
--
-- > personForm :: Form Person
--
-- For our purposes, it's good to see the long version.
personForm :: Html -> MForm Handler (FormResult Person, Widget)
personForm = renderDivs $ Person
    <$> areq textField     "Username: " Nothing
    <*> areq passwordField "Password: " Nothing

newAccountForm :: Html -> MForm Handler (FormResult NewPerson, Widget)
newAccountForm = renderDivs $ NewPerson
    <$> areq textField     "Username: " Nothing
    <*> areq passwordField "Password: " Nothing
    <*> areq passwordField "Confirm: " Nothing

-- The GET handler displays the form
getHomeR :: Handler Html
getHomeR = do
    -- Generate the form to be displayed
    (widget, enctype) <- generateFormPost personForm
    defaultLayout
        [whamlet|
            <h1>Login to Pong Web App</h1>
            <a href=@{RegisterR}>Click to register for an acount!
            <p>
                Login with your username and password.
                <form method=post action=@{PersonR} enctype=#{enctype}>
                    ^{widget}
                    <button>Login
        |]

getRegisterR :: Handler Html
getRegisterR = do
    -- Generate the form to be displayed
    (widget, enctype) <- generateFormPost newAccountForm
    defaultLayout
        [whamlet|
            <h1>Register to Pong Web App</h1>
            <form method=post action=@{PersonR} enctype=#{enctype}>
                ^{widget}
                <button>Register
        |]

-- The POST handler processes the form. If it is successful, it displays the
-- parsed person. Otherwise, it displays the form again with error messages.
postPersonR :: Handler Html
postPersonR = do
    ((result, widget), enctype) <- runFormPost personForm
    case result of
        FormSuccess person -> defaultLayout [whamlet|<p>#{show person}|]
        _ -> defaultLayout
            [whamlet|
                <h1>Uh oh, something went wrong with the POST request.</h1>
                <p>Invalid input, let's try again.
                <form method=post action=@{PersonR} enctype=#{enctype}>
                    ^{widget}
                    <button>LoginAttempt2
            |]

main :: IO ()
main = warp 3000 App